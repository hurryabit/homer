/** Homer's WebAssembly Runtime
 * 
 * Provides the heap and stack memories plus the utilities
 * to manage them.
 */

#include "mini-printf.c"

// Data types
typedef unsigned int u32;
typedef int i32;
typedef long long i64;
typedef unsigned long long u64;
typedef unsigned char u8;
typedef unsigned char bool;
typedef i64 ptr_t;

#define NULL ((void*)0)

// Heap storage
static const u32 WASM_PAGE_SIZE = 65536;
static const u32 INITIAL_HEAP_SIZE = 16 * WASM_PAGE_SIZE;
static const u32 INITIAL_STACK_SIZE = 16 * WASM_PAGE_SIZE / sizeof(ptr_t);
static const u32 MAX_HEAP_SIZE = WASM_PAGE_SIZE * 128;

static u32 heap_size = INITIAL_HEAP_SIZE;
static u8* from = NULL;
static u8* to = NULL;
u8* hp = NULL;
static u8* heap_limit = NULL;
static const u32 heap_headroom = 128;

static u8* mem_start = NULL;
static u8* mem_end = NULL;
static u32 mem_size = 0;

// Bitmap for keeping track of allocated blocks. Encompasses
// both from and to spaces.
// FIXME: We assume all blocks begin at 64-bit boundary, make
// sure this holds!
// FIXME: Make the bitmap growable.
static u64 *bitmap = NULL;
static u64 *bitmap_end = NULL;

// Stack storage
static u32 stack_size = INITIAL_STACK_SIZE;
static ptr_t* stack_start = NULL;
static ptr_t* stack_limit = NULL;
ptr_t* sp = NULL;
static const u32 stack_headroom = 32;
static u32 max_stack = 0;

// Pointer to the closure we're currently invoking.
// TODO consider removing this.
static struct closure* current_closure = (struct closure*)0;

// Heap block tags
typedef enum {
  T_MIN = 1,
  T_CLOSURE = 3,
  T_VARIANT0 = 4,
  T_VARIANT1 = 5,
  T_RECORD = 6,
  T_FWD = 7,
  T_MAX = T_FWD,
} tag_t;

// Abort reasons
enum {
  ABORT_ASSERT = 0,
  ABORT_ASSERT_STACKEMPTY = 1,
  ABORT_ASSERT_HEAPEMPTY = 2,
  ABORT_ASSERT_I32 = 3,
  ABORT_ASSERT_BOXED_I32 = 4,
  ABORT_OUT_OF_MEMORY = 5,
  ABORT_CURRENT_CLOSURE_UNSET = 6,
  ABORT_BAD_BLOCK = 7,
  ABORT_FWD_IN_TO_SPACE = 8,
  ABORT_ERROR = 9,
  ABORT_UNIMPLEMENTED = 90,

  ABORT_EXPECTED_T = 100,
  // ...
  ABORT_EXPECTED_T_MAX = ABORT_EXPECTED_T + T_MAX, 
};

// Host functions.
#define memcpy __builtin_memcpy
#define memmove __builtin_memmove

void log_i32(i32 x) __attribute__((__import_module__("host"), __import_name__("log_i32")));
void log_str(ptr_t off, i32 len) __attribute__((__import_module__("host"), __import_name__("log_str")));
void abort_with(i32 reason) __attribute__((__import_module__("host"), __import_name__("abort")));
void error() { abort_with(ABORT_ERROR); }

static char log_buf[1024];

#define logf(fmt, ...) do { \
  int __n__ = mini_snprintf(log_buf, sizeof(log_buf), fmt, __VA_ARGS__); \
  log_str((ptr_t)log_buf, __n__); \
} while(0)

// Forward declarations.
void garbage_collect();
void grow();

// Closure: pointer to an entry in the function table and the enclosed
// variables.
struct closure {
  u32 funcidx;
  u8 nvars; // TODO: redundant, can be calculated from size.
  ptr_t vars[0];
};

// Variant: rank (e.g. which variant) and a payload (if any).
// Currently we don't distinquish between variants which have a payload
// and which do not. The payload is uninitialized if the variant does not have
// a payload.
struct variant {
  u32 rank;
  ptr_t payload;
};

struct record {
  u8 nfields;
  ptr_t fields[0];
};

// Block: Heap is divided into blocks which carry the tag and size of the object.
struct block {
  u32 tag:8;
  u32 size:24;

  // FIXME(JM): Block size larger than strictly necessary, though this is more convenient.
  union {
    ptr_t fwd;
    struct closure clo;
    struct variant variant;
    struct record record;
  } data;
};

#define BLK(p) ((struct block*)(p))
#define BLK_HDR_SIZE sizeof(struct block)
#define PTR(p) ((ptr_t)p)

struct block* get_blk(ptr_t p, tag_t tag) {
  struct block* blk = BLK(p);
  if (blk->tag != tag) {
    logf("get_blk fail, expected block with tag %d, got %d, ptr = 0x%x", tag, blk->tag, p);
    abort_with(ABORT_EXPECTED_T + tag);
  }
  return blk;
}

void push(ptr_t p) { sp--; *sp = p; }
ptr_t pop() { return *sp++; }
void pop_() { sp++; }
void dup() { *(sp - 1) = *sp; sp--; }

void load(i32 off) {
  *(sp - 1) = *(sp + off);
  sp--;
}

ptr_t get(i32 off) {
  return sp[off];
}

i32 get32(i32 off) {
  logf("get32(%d) -> %d", off, sp[off]);
  return (i32)sp[off];
}


#define assert(cond) do { \
  if (!(cond)) { \
    logf("assertion failed at %s (at %s:%d)", __func__, __FILE__, __LINE__); \
    abort_with(ABORT_ASSERT); \
  } \
} while(0)

void assert_stackempty() {
  if (sp != stack_start) {
    abort_with(ABORT_ASSERT_STACKEMPTY);
  }
}

void assert_heapempty() {
  garbage_collect();
  if (hp != from) {
    abort_with(ABORT_ASSERT_HEAPEMPTY);
  }
}

void assert_i32(i32 x) {
  if (*sp != x) {
    abort_with(ABORT_ASSERT_I32);
  }
}

void assert_closure(struct block *blk) {
  if (blk->tag != T_CLOSURE) {
    abort_with(ABORT_EXPECTED_T + T_CLOSURE);
  }
}

void assert_variant(struct block *blk) {
  if (blk->tag != T_VARIANT0 && blk->tag != T_VARIANT1) {
    abort_with(ABORT_EXPECTED_T + T_VARIANT0);
  }
}
// TODO: use WASM's 'memory.copy'?
// Clang 8 compiles __builtin_memcpy as call to env.memcp
// Clang 9 might do the right thing.
// OTOH this is more portable.
void *memcopy(void *dst, const void *src, u32 n) {
  u8 *dst_ = (u8*)dst; u8 *src_ = (u8*)src;
  while (n--) {
    *dst_++ = *src_++;
  }
  return dst;
}

void needheap(u32 n) {
  if (hp + n + heap_headroom > heap_limit) {
    garbage_collect();
  }
  if (hp + n + heap_headroom > heap_limit) {
    grow();
  }

  // FIXME check here to make sure heap allocations
  // are 64-bit aligned for 'bitmap'.
  //logf("hp = %x", (u64)hp);
  assert(((u32)hp & 0x7) == 0);
}

void check_stack() {
  if (sp - stack_headroom < stack_limit) {
    grow();
  }

  u32 height = (ptr_t*)mem_end - sp;
  if (height > max_stack) {
    max_stack = height;
  }
}

bool bitmap_get(u32 bit) { 
  u64 *w = bitmap + (bit >> 6);
  if (w < bitmap || w >= bitmap_end)
    return 0;
  else
    return (*w & (1LL << (bit & 0x3f))) != 0; 
}
void bitmap_unset(u32 bit) { bitmap[bit >> 6] &= ~(1LL << (bit & 0x3f)); }
void bitmap_set(u32 bit) { bitmap[bit >> 6] |= 1LL << (bit & 0x3f); }
bool is_allocated(ptr_t p) { return bitmap_get((p - (u32)mem_start) >> 3); }
void set_allocated(ptr_t p) { bitmap_set((p - (u32)mem_start) >> 3); }
void unset_allocated(ptr_t p) { bitmap_unset((p - (u32)mem_start) >> 3); }

struct block* _alloc(tag_t tag, u32 block_size) {
  logf("_alloc tag=%d, block_size=%d", tag, block_size);
  needheap(block_size);
  assert((PTR(hp) & 0x7) == 0); // double-check alignment
  struct block* blk = BLK(hp);
  hp += block_size;
  assert(!is_allocated(PTR(blk)));
  set_allocated(PTR(blk));
  blk->tag = tag;
  blk->size = block_size;
  return blk;
}

void alloc_variant_0(i32 rank) {
  struct block* blk = _alloc(T_VARIANT0, BLK_HDR_SIZE);
  blk->data.variant.rank = rank;
  push(PTR(blk));
}

void alloc_variant(i32 rank, i32 payload) {
  struct block* blk = _alloc(T_VARIANT1, BLK_HDR_SIZE);
  blk->data.variant.rank = rank;
  blk->data.variant.payload = *(sp + payload);
  push(PTR(blk));
}

void alloc_closure(u32 funcidx, u32 nvars) {
  logf("alloc_closure fn %d, nvars %d", funcidx, nvars);
  struct block* blk = _alloc(T_CLOSURE, BLK_HDR_SIZE + sizeof(ptr_t)*nvars);
  blk->data.clo.funcidx = funcidx;
  blk->data.clo.nvars = nvars;
  push(PTR(blk));
}

void set_var(u32 var, u32 off) {
  struct block* blk = get_blk(*sp, T_CLOSURE);
  assert(var >= 0 && var < blk->data.clo.nvars);
  logf("set_var(%d, %d) = %d", var, off, *(sp + off));
  blk->data.clo.vars[var] = *(sp + off);
}

void alloc_record(u32 nfields) {
  struct block* blk = _alloc(T_RECORD, BLK_HDR_SIZE + sizeof(ptr_t)*nfields);
  blk->data.record.nfields = nfields;
  push(PTR(blk));
}

void set_field(u32 field, u32 off) {
  struct block* blk = get_blk(*sp, T_RECORD);
  assert(field >= 0 && field < blk->data.record.nfields);
  blk->data.record.fields[field] = *(sp + off);
}

void get_field(u32 record, u32 field) {
  struct block* blk = get_blk(sp[record], T_RECORD);
  assert(field >= 0 && field < blk->data.record.nfields);
  push(blk->data.record.fields[field]);
}

// Prepare for closure application by copying the captured variables to top of stack and returning
// the function index.
u32 prep_app_closure(u32 off) {
  struct block* blk = get_blk(sp[off], T_CLOSURE);
  for (int i = 0; i < blk->data.clo.nvars; i++) {
    logf("prep_app_closure: pushed %d", blk->data.clo.vars[i]);
    *--sp = blk->data.clo.vars[i];
  }
  current_closure = &blk->data.clo;
  return blk->data.clo.funcidx;
}

void push_param(u32 off) {
  // TODO: Remove the current_closure hack. We'd need to otherwise save and restore it. Not worth it
  // right now.
  if (!current_closure) { abort_with(ABORT_CURRENT_CLOSURE_UNSET); }
  logf("push_param: pushed %d", 
    sp[off + current_closure->nvars]);
  push(sp[off + current_closure->nvars] /* captured variables */);
}

u32 get_funcidx() {
  struct block *blk = get_blk(*sp, T_CLOSURE);
  return blk->data.clo.funcidx;
}

#define B(exp) ((exp) ? 1LL : 0LL)

void add(i32 a, i32 b) { push(sp[a] + sp[b]); }
void sub(i32 a, i32 b) { push(sp[a] - sp[b]); }
void div(i32 a, i32 b) { push(sp[a] / sp[b]); }
void mul(i32 a, i32 b) { push(sp[a] * sp[b]); }
void greater(i32 a, i32 b) { push(B(sp[a] > sp[b])); }
void greater_eq(i32 a, i32 b) { push(B(sp[a] >= sp[b]));  }
void less(i32 a, i32 b) { push(B(sp[a] < sp[b])); }
void less_eq(i32 a, i32 b) { push(B(sp[a] <= sp[b])); }
void not_eq(i32 a, i32 b) { push(B(sp[a] != sp[b])); }
void equals(i32 a, i32 b) { push(B(sp[a] == sp[b])); }

void loadenv(u32 off, u32 idx) {
  struct block *blk = get_blk(*(sp + off), T_CLOSURE);
  push(blk->data.clo.vars[idx]);
}

u32 get_rank(u32 off) {
  struct block *blk = BLK(*(sp + off));
  assert_variant(blk);
  return blk->data.variant.rank;
}


void load_payload(u32 off) {
  struct block *blk = BLK(*(sp + off));
  assert_variant(blk);
  push(blk->data.variant.payload);
}

// Return from a function by shifting out the frame and leaving
// the return value on top.
void ret(u32 n) {
  ptr_t result = *sp;

  sp += n;
  *sp = result;

  logf("ret(%d), sp = %d, result = %d", n, sp, result);

  assert(sp < stack_start && sp > stack_limit);

  logf("ret: exit", 0);

  check_stack(); // TODO should be invoked when applying!
}

// Shift arguments over the current frame for tail-call optimization
void shift(u32 frame_size, u32 nargs) {
  logf("shift(%d, %d)", frame_size, nargs);
  // stack: <args> <bindings> ...
  for (int i = nargs - 1; i >= 0; i--) {
    sp[frame_size + i] = sp[i];
  }
  sp += frame_size;
  // stack: <args> ...
}

// Self tests
void selftest() {
  // TODO
}

// Initialize the runtime
void init() {
  // Allocate memory for the allocation bitmap.
  {
    u32 bitmap_size_bytes = MAX_HEAP_SIZE / 64;
    u32 rounding = bitmap_size_bytes % WASM_PAGE_SIZE; // FIXME: get rid of this.
    i32 orig_mem_size_pages = __builtin_wasm_memory_grow(0, (bitmap_size_bytes + rounding) / WASM_PAGE_SIZE);
    bitmap = (u64*)(orig_mem_size_pages * WASM_PAGE_SIZE);
    bitmap_end = bitmap + bitmap_size_bytes;
  }

  // Allocate heap and stack.
  mem_size = heap_size * 2 + stack_size * sizeof(ptr_t);
  {
    i32 orig_mem_size_pages = __builtin_wasm_memory_grow(0, mem_size / WASM_PAGE_SIZE);
    assert(orig_mem_size_pages >= 0);
    mem_start = (u8*) (orig_mem_size_pages * WASM_PAGE_SIZE);
  }
  mem_end = mem_start + mem_size;
  from = mem_start;
  to = mem_start + heap_size;
  hp = mem_start;
  heap_limit = from + heap_size;
  stack_start = (ptr_t*) mem_end;
  sp = stack_start;
  stack_limit = (ptr_t*) (mem_end - stack_size);

  logf("init allocated %lu bytes (%lu pages)", mem_size, mem_size / WASM_PAGE_SIZE);

  logf("mem_start = 0x%x, sp = 0x%0x, hp = 0x%x, bitmap = 0x%x",
    (u32)mem_start, (u32)sp, (u32)hp, (u32)bitmap);

  selftest();
}

// Grow the amount of allocated memory
void grow() {
  logf("grow: enter", 0);

  if (from > to) {
    // from-space is after the to-space which makes extending non-trivial.
    // run another garbage collection to swap the spaces.
    garbage_collect();
  }
  assert(from < to);

  if (mem_size * 2 > MAX_HEAP_SIZE) {
    // FIXME: This check is here just because growing of "bitmap"
    // has not been implemented.
    abort_with(ABORT_OUT_OF_MEMORY);
  }

  if (__builtin_wasm_memory_grow(0, mem_size / WASM_PAGE_SIZE) < 0) {
    abort_with(ABORT_OUT_OF_MEMORY);
  }

  to += heap_size;
  heap_limit += heap_size;
  u32 orig_mem_size = mem_size;
  mem_size *= 2;
  heap_size *= 2;
  stack_size *= 2;
  mem_end = mem_start + mem_size;

  u32 stack_used = stack_start - sp;
  stack_start = (ptr_t*)mem_end;
  ptr_t* newsp = stack_start - stack_used;
  memcopy(newsp, sp, stack_used * sizeof(ptr_t));
  assert(*sp == *newsp);
  sp = newsp;
  stack_limit = stack_start - stack_size;

  logf("grow: %lu -> %lu", orig_mem_size, mem_size);
}

// Copy block from 'from' space to 'to' space and return the new block location.
ptr_t copy_block(ptr_t p, u8 **to_end) {
  if (!is_allocated(p)) {
    // p wasn't a pointer, so return it as-is.
    return p;
  }

  struct block *blk = BLK(p);

  if (blk->tag == T_FWD) {
    // Block has already been copied, return its new address.
    return blk->data.fwd;
  } else {
    if (blk->tag < T_MIN || blk->tag > T_MAX) {
      abort_with(ABORT_BAD_BLOCK);
    }

    // Copy the block and set up forwarding.
    ptr_t newp = PTR(*to_end);
    set_allocated(newp);
    memcopy(*to_end, blk, blk->size);
    *to_end += blk->size;

    blk->tag = T_FWD;
    blk->data.fwd = newp;
    return newp;
  }
}

void garbage_collect() {
  logf("garbage_collect: enter", 0);
  //abort_with(ABORT_OUT_OF_MEMORY);

  u32 before = hp - from;

  // Copy the blocks reachable from stack to the 'to' space.
  u8 *to_end = to;
  {
    ptr_t *spt = sp;
    while (spt < stack_start) {
      *spt = copy_block(*spt, &to_end);
      spt++;
    }
  }

  // Traverse through the new blocks in the 'to' space and copy
  // referenced blocks from the 'from' space to the 'to' space.
  u8 *top = to;
  while (top < to_end) {
    struct block *blk = BLK(top);

    switch (blk->tag) {
      case T_CLOSURE: {
        struct closure *clo = &blk->data.clo;
        for (int i = 0; i < clo->nvars; i++) {
          clo->vars[i] = copy_block(clo->vars[i], &to_end);
        }
        break;
      }

      case T_VARIANT1: {
        struct variant *v = &blk->data.variant;
        v->payload = copy_block(v->payload, &to_end);
        break;
      }

      case T_RECORD: {
        struct record *rec = &blk->data.record;
        for (int i = 0; i < rec->nfields; i++) {
          rec->fields[i] = copy_block(rec->fields[i], &to_end);
        }
        break;
      }

      case T_VARIANT0:
        break;

      case T_FWD:
        abort_with(ABORT_FWD_IN_TO_SPACE);

      default:
        abort_with(ABORT_UNIMPLEMENTED);
    }
    top += blk->size;
  }

  // Clear all allocations in the now unused 'from' space.
  ptr_t p = (ptr_t)from;
  while (p < (ptr_t)hp) {
    unset_allocated(p);
    p += BLK(p)->size;
  }

  // Swap spaces.
  hp = to_end;
  {
    u8 *tmp = from;
    from = to;
    to = tmp;
  }
  heap_limit = from + heap_size;

  u32 after = hp - from;
  logf("garbage collect: %lu -> %lu", before, after);


// XXX
  logf("max stack: %lu", max_stack);
}


// XXX testing "extern fn"
/*
// Host functions.
void host_set_pixel(i32 x, i32 y, i32 c) __attribute__((__import_module__("host"), __import_name__("set_pixel")));

void set_pixel() {
    i32 c = (i32)get_blk(pop(), T_I64)->data.i64;
    i32 y = (i32)get_blk(pop(), T_I64)->data.i64;
    i32 x = (i32)get_blk(pop(), T_I64)->data.i64;
    host_set_pixel(x, y, c);
    alloc_i32(0);
}
*/
