#include <assert.h>
#include <stdlib.h>

#define o_new(st) {                             \
    st = malloc(sizeof(struct state_t));        \
    st->a = -1; st->b = -1; }

#define o_add(st,rv,v) {                                                \
    if ((st)->a == -1 && (st)->b == -1) { (st)->a = v; rv = 1; } \
    else if ((st)->a != -1 && (st)->b == -1) { (st)->b = v; rv = 1; } \
    else if ((st)->a == -1 && (st)->b != -1) { (st)->a = v; rv = 1; } \
    else { rv = 0; } }

#define o_isin(st,rv,v) {                       \
    rv = 0;                                     \
    if ((st)->a == v) rv = 1;                   \
    if ((st)->b == v) rv = 1; }

#define o_clear(st,rv) { (st)->a = -1; (st)->b = -1; rv = 1; }

#define o_norm(st,rv) {                         \
    if ((st)->a > (st)->b) {                    \
      int t = (st)->b;                          \
      (st)->b = (st)->a;                        \
      (st)->a = t;                              \
    }                                           \
    rv = 0; }

