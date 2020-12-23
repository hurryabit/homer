
/** Homer's WebAssembly Runtime
 * 
 * Provides the heap and stack memories plus the utilities
 * to manage them.
 */

// Data types
typedef unsigned int u32;
typedef __UINTPTR_TYPE__ ptr_t;
typedef int i32;
typedef long long i64;
typedef unsigned char u8;

// Heap storage
#define HEAP_SIZE 65536
static u8 memA[HEAP_SIZE];
static u8 memB[HEAP_SIZE];
static u8* from = memA;
static u8* to = memB;
static u8* hp = memA;
static u8* mem_end = &memA[HEAP_SIZE];

// TODO: Consider using __builtin_wasm_memory_grow() to resize heap & stack.

// Stack storage
#define STACK_SIZE 1024
static ptr_t stack[STACK_SIZE];
static ptr_t* stack_start = &stack[STACK_SIZE];
//static ptr_t* stack_end = &stack[0];
static ptr_t* sp = &stack[STACK_SIZE];

// Pointer to the closure we're currently invoking.
// TODO consider removing this.
static struct closure *current_closure = (struct closure*)0;

// Heap block tags
typedef enum {
  T_MIN = 1,
  T_I32 = T_MIN,
  T_I64 = 2,
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
void log_i32(i32 x) __attribute__((__import_module__("host"), __import_name__("log_i32")));
void abort_with(i32 reason) __attribute__((__import_module__("host"), __import_name__("abort")));
void error() { abort_with(ABORT_ERROR); }

// Forward declarations.
void garbage_collect();


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

// Record
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
    i32 i32;
    i64 i64;
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
  struct block *blk = BLK(p);
  if (blk->tag != tag) {
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

void assert(i32 cond) {
  if (!cond) abort_with(ABORT_ASSERT);
}

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

void assert_boxed_i32(i32 x) {
  struct block* blk = get_blk(*sp, T_I32);
  if (blk->data.i32 != x) {
    abort_with(ABORT_ASSERT_BOXED_I32);
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

void needheap(u32 n) {
  if (hp + n > mem_end) {
    garbage_collect();
  }
  if (hp + n > mem_end) {
    abort_with(ABORT_OUT_OF_MEMORY);
  }
}

// FIXME: Should probably have a naming convention that tells whether the function
// pushes to homer stack or wasm stack.
void alloc_i32(i32 x) {
  needheap(BLK_HDR_SIZE);
  struct block *blk = BLK(hp);
  blk->tag = T_I32;
  blk->size = BLK_HDR_SIZE;
  blk->data.i32 = x;
  hp += blk->size;
  push(PTR(blk));
}

void alloc_i64(i64 x) {
  needheap(BLK_HDR_SIZE);
  struct block *blk = BLK(hp);
  blk->tag = T_I64;
  blk->size = BLK_HDR_SIZE;
  blk->data.i64 = x;
  hp += blk->size;
  push(PTR(blk));
}

void alloc_variant_0(i32 rank) {
  needheap(BLK_HDR_SIZE + sizeof(u32));
  struct block *blk = BLK(hp);
  blk->tag = T_VARIANT0;
  blk->size = BLK_HDR_SIZE;
  blk->data.variant.rank = rank;
  hp += blk->size;
  push(PTR(blk));
}

void alloc_variant(i32 rank, i32 payload) {
  needheap(BLK_HDR_SIZE + sizeof(u32));
  struct block *blk = BLK(hp);
  blk->tag = T_VARIANT1;
  blk->size = BLK_HDR_SIZE;
  blk->data.variant.rank = rank;
  if (payload >= 0) {
    blk->data.variant.payload = *(sp + payload);
  }
  hp += blk->size;
  push(PTR(blk));
}

void alloc_closure(u32 funcidx, u32 nvars) {
  needheap(BLK_HDR_SIZE + sizeof(u32)*nvars);
  struct block *blk = BLK(hp);
  blk->tag = T_CLOSURE;
  blk->size = BLK_HDR_SIZE + sizeof(u32)*nvars;
  blk->data.clo.funcidx = funcidx;
  blk->data.clo.nvars = nvars;
  hp += blk->size;
  push(PTR(blk));
}

void set_var(u32 var, u32 off) {
  struct block *blk = get_blk(*sp, T_CLOSURE);
  assert(var >= 0 && var < blk->data.clo.nvars);
  blk->data.clo.vars[var] = *(sp + off);
}

void alloc_record(u32 nfields) {
  needheap(BLK_HDR_SIZE + sizeof(u32)*nfields);
  struct block *blk = BLK(hp);
  blk->tag = T_RECORD;
  blk->size = BLK_HDR_SIZE + sizeof(u32)*nfields;
  blk->data.record.nfields = nfields;
  hp += blk->size;
  push(PTR(blk));
}

void set_field(u32 field, u32 off) {
  struct block *blk = get_blk(*sp, T_RECORD);
  assert(field >= 0 && field < blk->data.record.nfields);
  blk->data.record.fields[field] = *(sp + off);
}

void get_field(u32 record, u32 field) {
  struct block *blk = get_blk(*(sp + record), T_RECORD);
  assert(field >= 0 && field < blk->data.record.nfields);
  push(blk->data.record.fields[field]);
}

// Prepare for closure application by copying the captured variables to top of stack and returning
// the function index.
u32 prep_app_closure(u32 off) {
  struct block *blk = get_blk(*(sp + off), T_CLOSURE);
  for (int i = 0; i < blk->data.clo.nvars; i++) {
    *--sp = blk->data.clo.vars[i];
  }
  current_closure = &blk->data.clo;
  return blk->data.clo.funcidx;
}

void push_param(u32 off) {
  // TODO: Remove the current_closure hack. We'd need to otherwise save and restore it. Not worth it
  // right now.
  if (!current_closure) { abort_with(ABORT_CURRENT_CLOSURE_UNSET); }
  push(*(sp + off + current_closure->nvars /* captured variables */));
}

u32 get_funcidx() {
  struct block *blk = get_blk(*sp, T_CLOSURE);
  return blk->data.clo.funcidx;
}

void add(i32 a, i32 b) {
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i64(blk_a->data.i64 + blk_b->data.i64);
}
void sub(i32 a, i32 b) { 
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i64(blk_a->data.i64 - blk_b->data.i64);
}

void div(i32 a, i32 b) { 
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i64(blk_a->data.i64 / blk_b->data.i64);
}

void mul(i32 a, i32 b) { 
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i64(blk_a->data.i64 * blk_b->data.i64);
}

void greater(i32 a, i32 b) { 
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i32(blk_a->data.i64 > blk_b->data.i64);
}

void greater_eq(i32 a, i32 b) { 
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i32(blk_a->data.i64 >= blk_b->data.i64);
}

void less(i32 a, i32 b) { 
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i32(blk_a->data.i64 < blk_b->data.i64);
}

void less_eq(i32 a, i32 b) { 
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i32(blk_a->data.i64 <= blk_b->data.i64);
}

void not_eq(i32 a, i32 b) {
  struct block *blk_a = get_blk(*(sp + a), T_I64);
  struct block *blk_b = get_blk(*(sp + b), T_I64);
  alloc_i32(blk_a->data.i64 != blk_b->data.i64);
}

void equals(i32 a, i32 b) { 
  struct block *blk_a = BLK(*(sp + a));
  struct block *blk_b = BLK(*(sp + b));
  // TODO structural equality.
  if (blk_a->tag == T_I64 && blk_b->tag == T_I64) {
    push(blk_a->data.i64 == blk_b->data.i64);
  } else if (blk_a->tag == T_I32 && blk_b->tag == T_I32) {
    push(blk_a->data.i32 == blk_b->data.i32);
  } else {
    push(blk_a == blk_b);
  }
 }

void loadenv(u32 off, u32 idx) {
  struct block *blk = get_blk(*(sp + off), T_CLOSURE);
  push(blk->data.clo.vars[idx]);
}

i32 deref_i32() {
  return get_blk(pop(), T_I32)->data.i32;
}

i64 deref_i64() {
  return get_blk(pop(), T_I64)->data.i64;
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
}

// TODO: use WASM's 'memory.copy' ? LLVM memcpy builtin doesn't seem to work.
void *memcopy(void *dst, const void *src, u32 n) {
  u8 *dst_ = (u8*)dst; u8 *src_ = (u8*)src;
  while (n--) {
    *dst_++ = *src_++;
  }
  return dst;
}

// Copy block from 'from' space to 'to' space and return the new block location.
ptr_t copy_block(ptr_t p, u8 **to_end) {
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
    memcopy(*to_end, blk, blk->size);
    *to_end += blk->size;

    blk->tag = T_FWD;
    blk->data.fwd = newp;
    return newp;
  }
}

void garbage_collect() {
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
      case T_I32:
      case T_I64:
        break;
      case T_FWD:
        abort_with(ABORT_FWD_IN_TO_SPACE);

      default:
        abort_with(ABORT_UNIMPLEMENTED);
    }
    top += blk->size;
  }

  // Swap spaces.
  hp = to_end;
  mem_end = to + HEAP_SIZE;

  {
    u8 *tmp = from;
    from = to;
    to = tmp;
  }
}
