
typedef unsigned int u32;
typedef unsigned int ptr_t; // TODO: where to get the pointer size?
typedef int i32;
typedef unsigned char u8;


// Heap storage
#define MEM_SIZE 1024
static u8 memA[MEM_SIZE];
static u8 memB[MEM_SIZE];
static u8* from = memA;
static u8* to = memB;
static u8* hp = memA;
static u8* mem_end = memA + MEM_SIZE;

// Stack storage
#define STACK_SIZE 1024
static u32 stack[STACK_SIZE]; // FIXME should be ptr_t
static u32* stack_end = &stack[STACK_SIZE];
static u32* sp = &stack[STACK_SIZE];

typedef enum {
  T_I32 = 0,
  T_CLOSURE = 1,
  T_FWD = 2
} tag_t;

struct closure {
  u32 funcidx;
  u8 nvars;
  u32 vars[0];
};

struct block {
  u32 tag:8;
  u32 size:24;

  // FIXME(JM): Block size larger than necessary.
  union {
    i32 i32;
    ptr_t fwd;
    struct closure clo;
  } data;
};

#define BLK(p) ((struct block*)(p))
#define BLK_HDR_SIZE sizeof(struct block)
#define PTR(p) ((ptr_t)p)

void push(u32 p) { sp--; *sp = p; }
u32 pop() { return *sp++; }
void pop_() { sp++; }
void dup() { *(sp - 1) = *sp; sp--; }

void __abort__() {
  // TODO better abort
  (*((u8*)999999999))++;
}

void assert_stackempty() {
  if (sp != stack_end) {
    (*((u8*)666666666))++;
    __abort__();
  }
}

void assert_i32(i32 x) {
  if (*sp != x) {
    (*((u8*)(90000000+*sp)))++;
  }
}

void assert_boxed_i32(i32 x) {
  struct block* blk = BLK(*sp);
  if (blk->tag != T_I32) {
    (*((u8*)787878787))++;
  }
  if (blk->data.i32 != x) {
    (*((u8*)(70000000+blk->data.i32)))++;
  }
}

void gc();
void needheap(u32 n) {
  if (hp + n > mem_end) {
    gc();
  }
  if (hp + n > mem_end) {
    __abort__();
  }
}

void alloc_i32(i32 x) {
  needheap(BLK_HDR_SIZE);
  struct block *blk = BLK(hp);
  blk->tag = T_I32;
  blk->size = BLK_HDR_SIZE;
  blk->data.i32 = x;
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
  for (int i = 0; i < nvars; i++) {
    // FIXME: which way around should we store the variables?
    blk->data.clo.vars[i] = *sp++;
  }
  hp += blk->size;
  push(PTR(blk));
}

void add() { alloc_i32(BLK(pop())->data.i32 + BLK(pop())->data.i32); }
void sub() { alloc_i32(BLK(pop())->data.i32 - BLK(pop())->data.i32); }

void loadenv(u32 off, u32 idx) {
  struct block *blk = BLK(*(sp + off));
  if (blk->tag != T_CLOSURE) { __abort__(); };
  push(blk->data.clo.vars[idx]);
}

i32 deref_i32() {
  return BLK(pop())->data.i32;
}

void ret(u32 nargs) {
  u32 result = *sp;
  sp += nargs + 1;
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
    if (blk->tag != T_CLOSURE && blk->tag != T_I32) __abort__();

    // Copy the block and set up forwarding.
    ptr_t newp = PTR(*to_end);
    memcopy(*to_end, blk, blk->size);
    *to_end += blk->size;

    blk->tag = T_FWD;
    blk->data.fwd = newp;
    return newp;
  }
}

void gc() {
  // Copy the blocks reachable from stack to the 'to' space.
  u8 *to_end = to;
  {
    u32 *spt = sp;
    while (spt < stack_end) {
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
      case T_I32:
        break;
      case T_FWD:
        __abort__();
    }
    top += blk->size;
  }

  // Swap spaces.
  hp = to_end;
  mem_end = to + MEM_SIZE;

  {
    u8 *tmp = from;
    from = to;
    to = tmp;
  }
}
