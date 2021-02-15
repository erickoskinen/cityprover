#define HTCAPACITY 11
struct entry_t { int key; int value; };
struct state_t { struct entry_t table[HTCAPACITY]; int keys; };
int slot;

#define asmKV(i) if (s1.table[i].key != -1) __VERIFIER_assume(s1.table[i].key == s2.table[i].key && s1.table[i].value == s2.table[i].value)

#define assume_ibeta() \
  __VERIFIER_assume(s1.keys >= 0);              \
  __VERIFIER_assume(s1.keys == s2.keys);        \
  __VERIFIER_assume(slot >= 0 && slot < 11);\
__VERIFIER_assume(s1.table[slot].key == s2.table[slot].key && s1.table[slot].value == s2.table[slot].value); \
asmKV(0);					\
  asmKV(1);\
  asmKV(2);\
  asmKV(3);\
  asmKV(4);\
  asmKV(5);\
  asmKV(6);\
  asmKV(7);\
  asmKV(8);\
  asmKV(9);\
  asmKV(10);

#define assertKV(i) \
  if (s1.table[i].key != -1 && s1.table[i].key != s2.table[i].key) __VERIFIER_error(); \
  if (s1.table[i].key != -1 && s1.table[i].value != s2.table[i].value) __VERIFIER_error()

#define assert_ibeta()					\
  if (!( s1.keys == s2.keys )) __VERIFIER_error();			\
  if (s1.table[slot].key != s2.table[slot].key) __VERIFIER_error();      \
  if (s1.table[slot].value != s2.table[slot].value) __VERIFIER_error();      \
  assertKV(0);\
  assertKV(1);\
  assertKV(2);\
  assertKV(3);\
  assertKV(4);\
  assertKV(5);\
  assertKV(6);\
  assertKV(7);\
  assertKV(8);\
  assertKV(9);\
  assertKV(10);


