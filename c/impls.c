#define STACK_SIZE 200
#define VALS stk->vals
#define IND stk->ind
#define STACK Stack *stk
#define POP() pop(stk)
#define POPI() popi(stk)
#define POPD() popd(stk)
#define POPC() popc(stk)
#define POPA() popa(stk)
#define PUSH(val) push(stk, val)

/////////////////Begin utilities and stuff///////////////////

typedef enum
{
  INT,
  DOUBLE,
  CHAR,
  ARRAY
} Type;

typedef union
{
  int i;
  double d;
  char c;
  Array *a;
} Value_Union;

typedef struct {
  int size;
  Val **arr;
} Array;

typedef struct
{
  Type type;
  Value_Union value;
} Val;

typedef struct
{
  Val *vals[STACK_SIZE];
  int ind;
} Stack;

Val *pop(STACK)
{
  return VALS[IND--];
}

void *push(STACK, Val *val) {
  VALS[++IND] = val;
}

void free_val(Val *val) {
  if (val->type == ARRAY) {
    free(val->value.a->arr);
    free(val->value.a);
  }
  free(val);
}

int popi(STACK) {
  Val *popped = pop(stk);
  int res = popped->value.i;
  free(popped);
  return res;
}

double popd(STACK) {
  Val *popped = pop(stk);
  double res = popped->value.d;
  free(popped);
  return res;
}

char popc(STACK) {
  Val *popped = pop(stk);
  char res = popped->value.c;
  free(popped);
  return res;
}

Array *popa(STACK) {
  Val *popped = pop(stk);
  Array *res = popped->value.a;
  free(popped);
  return res;
}

Array *make_arr(int len) {
  Array *arr;
  arr = malloc(sizeof(Array));
  arr->size = len;
  arr->arr = (Val *) calloc(len, sizeof(Value_Union));
  return arr;
}

/////////////////End utilities and stuff///////////////////

/////////////////////Begin stack-related builtins (prefix `~`)/////////////////////

void stk_dup(STACK)
{
  VALS[++IND] = VALS[IND];
}

void stk_swap(STACK)
{
  Val *temp = VALS[IND];
  VALS[IND] = VALS[IND - 1];
  VALS[IND - 1] = temp;
}

void stk_rot(STACK)
{
  Val *temp = VALS[IND];
  VALS[IND] = VALS[IND - 2];
  VALS[IND - 2] = VALS[IND - 1];
  VALS[IND - 1] = temp;
}

void stk_irot(STACK)
{
  Val *temp = VALS[IND];
  VALS[IND] = VALS[IND - 1];
  VALS[IND - 1] = VALS[IND - 2];
  VALS[IND - 2] = temp;
}

void stk_over(STACK)
{
  IND++;
  VALS[IND] = VALS[IND - 2];
}

void stk_tuck(STACK)
{
  Val *temp = VALS[IND - 1];
  VALS[IND - 1] = VALS[IND];
  VALS[IND + 1] = VALS[IND];
  VALS[IND] = temp;
  IND++;
}

void stk_discard(STACK)
{
  free(pop(stk));
}

//////////////////////End stack-related builtins///////////////////////////////

/////////////////////Begin array-related builtins (prefix `:`)/////////////////////

//a b -> a++b
void arr_append(STACK) {
  Array *first = pop(stk)->value.a, *second = pop(stk)->value.a;
  int size1 = first->size, size2 = second->size;
  Array *res = make_arr(size1 + size2);
  for (int i = 0; i < size2; i ++) {
    res->arr[i] = second->arr[i];
  }
  for (int i = 0; i < size1; i ++) {
    res->arr[i + size2] = first->arr[i];
  }
  PUSH(res);
}

//Pop I, then A, then push A[I]
void arr_index(STACK) {
  int i = POPI();
  Array *a = POPA();
  PUSH(a->arr[i]);
}

//Pop V, then I, then A, then set A[I] = V without pushing anything
void arr_set(STACK) {
  Val *v = POP();
  int i = POPI();
  Array *a = POPA();
  a->arr[i] = v;
}

void arr_make(STACK) {
  int n = POPI();
  Array *res = make_arr(n);
  for (int i = 0; i < n; i ++) {
    res->arr[i] = POP();
  }
  PUSH(res);
}

//////////////////////End array-related builtins///////////////////////////////
