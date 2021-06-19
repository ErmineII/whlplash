#define STACK_SIZE 200

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

typedef struct
{
  int size;
  Val **arr;
} Array;

typedef struct
{
  Type type;
  Value_Union value;
} Val;

const Val *vals[STACK_SIZE];
int ind = 0;

int tempi;
double tempd;
char tempc;
Array *tempa;
Val *tempv;

#define POP() vals[ind--];
#define PUSH(val) vals[++ind] = val;
#define PEEK() vals[ind];

#define FREE_VAL(val)          \
  {                            \
    if (val->type == ARRAY)    \
    {                          \
      free(val->value.a->arr); \
      free(val->value.a);      \
    }                          \
    free(val);                 \
  }

#define POPI()                \
  (                           \
      tempv = POP(),          \
      tempi = tempv->value.i, \
      free(tempv),            \
      tempi)

#define POPD()                \
  (                           \
      tempv = POP(),          \
      tempd = tempv->value.d, \
      free(tempv),            \
      tempd)

#define POPC()                \
  (                           \
      tempv = POP(),          \
      tempc = tempv->value.c, \
      free(tempv),            \
      tempc)

#define POPA()                \
  (                           \
      tempv = POP(),          \
      tempa = tempv->value.a, \
      free(tempv),            \
      tempa)

Array *made_arr;
#define MAKE_ARR(len)                                     \
  (                                                       \
      made_arr = malloc(sizeof(Array)),                        \
      made_arr->size = len,                                    \
      made_arr->arr = (Val *)calloc(len, sizeof(Value_Union)), \
      made_arr)

/////////////////End utilities and stuff///////////////////

/////////////////////Begin stack-related builtins (prefix `~`)/////////////////////

#define STK_DUP stk[++ind] = stk[ind];

#define STK_SWAP()             \
  {                            \
    Val *temp = vals[ind];     \
    vals[ind] = vals[ind - 1]; \
    vals[ind - 1] = temp;      \
  }

#define STK_ROT()                  \
  {                                \
    Val *temp = vals[ind];         \
    vals[ind] = vals[ind - 2];     \
    vals[ind - 2] = vals[ind - 1]; \
    vals[ind - 1] = temp;          \
  }

#define STK_IROT()                 \
  {                                \
    Val *temp = vals[ind];         \
    vals[ind] = vals[ind - 1];     \
    vals[ind - 1] = vals[ind - 2]; \
    vals[ind - 2] = temp;          \
  }

#define STK_OVER()             \
  {                            \
    ind++;                     \
    vals[ind] = vals[ind - 2]; \
  }

#define STK_TUCK()             \
  {                            \
    Val *temp = vals[ind - 1]; \
    vals[ind - 1] = vals[ind]; \
    vals[ind + 1] = vals[ind]; \
    vals[ind] = temp;          \
    ind++;                     \
  }

#define STK_DISCARD() free(pop(stk));

//////////////////////End stack-related builtins///////////////////////////////

/////////////////////Begin array-related builtins (prefix `:`)/////////////////////

//a b -> a++b
#define ARR_APPEND()                                            \
  {                                                             \
    Array *first = POP()->value.a, *second = pop(stk)->value.a; \
    int size1 = first->size, size2 = second->size;              \
    Array *res = make_arr(size1 + size2);                       \
    for (int i = 0; i < size2; i++)                             \
      res->arr[i] = second->arr[i];                             \
    for (int i = 0; i < size1; i++)                             \
      res->arr[i + size2] = first->arr[i];                      \
    PUSH(res);                                                  \
  }

//Pop I, then A, then push A[I]
#define ARR_INDEX()           \
  {                           \
    tempi = POPI();           \
    tempa = POPA();           \
    PUSH(tempa->arr[tempi])); \
  }

//Pop V, then I, then A, then set A[I] = V without pushing anything
#define ARR_SET()               \
  {                             \
    tempv = POP();              \
    tempi = POPI();             \
    tempa = POPA();             \
    tempa->arr[tempi] = tempv); \
  }

#define ARR_MAKE()                  \
  {                                 \
    tempi = POPI();                 \
    tempa = MAKE_ARR(tempi);        \
    for (int i = 0; i < tempi; i++) \
      tempa->arr[i] = POP();        \
    PUSH(tempa);                    \
  }

//////////////////////End array-related builtins///////////////////////////////
