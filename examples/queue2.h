#define MAXQUEUE 5


struct state_t { int a[MAXQUEUE]; int front; int rear; int size; };

#define assume_ibeta()                          \
  __VERIFIER_assume(s1.front >= 0);             \
  __VERIFIER_assume(s1.front <= 5 -1);\
  __VERIFIER_assume(s1.rear >= 0);    \
  __VERIFIER_assume(s1.rear <= 5 -1); \
  __VERIFIER_assume(s1.size >= 0);    \
  __VERIFIER_assume(s1.size < 5);                     \
  __VERIFIER_assume(s1.size == s1.rear - s1.front + 1); \
  __VERIFIER_assume(s1.front == s2.front);              \
  __VERIFIER_assume(s1.rear == s2.rear);                \
  __VERIFIER_assume(s1.size == s2.size);                                \
  for (int i = s1.front; i <= s1.rear; i++) { __VERIFIER_assume(s1.a[i] == s2.a[i]); } \
  __VERIFIER_assume(s1.rear >= s1.front - 1);
 

#define assert_ibeta() \
  if (!( s1.rear >= s1.front - 1 )) __VERIFIER_error(); \
  if (!( s1.front == s2.front )) __VERIFIER_error(); \
  if (!( s1.rear == s2.rear )) __VERIFIER_error();\
  if (!( s1.size == s2.size )) __VERIFIER_error();\
 for (int i = s1.front; i <= s1.rear; i++) { \
   if (s1.a[i] != s2.a[i]) __VERIFIER_error(); \
 }
