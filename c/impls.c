#define STACK_SIZE 200
#define VALS stk->vals
#define IND stk->ind

typedef union {
  int num;
  void* other;
} Val;

typedef struct {
  Val vals[STACK_SIZE];
  int ind;
} Stack;

typedef struct {
  Val data;
  LinkedList *next;
} LinkedList;

/////////////////////Begin stack-related methods (prefix `~`)/////////////////////

void stk_dup(Stack *stk) {
  VALS[IND + 1] = VALS[IND];
  IND ++;
}

void stk_swap(Stack *stk) {
  Val temp = VALS[IND];
  VALS[IND] = VALS[IND - 1];
  VALS[IND - 1] = temp;
}

void stk_rot(Stack *stk) {
  Val temp = VALS[IND];
  VALS[IND] = VALS[IND - 2];
  VALS[IND - 2] = VALS[IND - 1];
  VALS[IND - 1] = temp;
}

void stk_irot(Stack *stk) {
  Val temp = VALS[IND];
  VALS[IND] = VALS[IND - 1];
  VALS[IND - 1] = VALS[IND - 2];
  VALS[IND - 2] = temp;
}

void stk_over(Stack *stk) {
  IND ++;
  VALS[IND] = VALS[IND - 2];
}

void stk_tuck(Stack *stk) {
  Val temp = VALS[IND - 1];
  VALS[IND - 1] = VALS[IND];
  VALS[IND + 1] = VALS[IND];
  VALS[IND] = temp;
  IND ++;
}

//////////////////////End stack-related methods///////////////////////////////
