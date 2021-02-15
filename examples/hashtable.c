#include <stdlib.h>

#define o_new(st) { s1 = malloc(sizeof(struct state_t)); \
    for (int i=0;i<HTCAPACITY;i++) { s1->table[i].key = -1; } \
    s1->keys = 0; }

#define o_put(st,rv,k,v) { int slot = k % HTCAPACITY; \
    if(st.table[slot].key == -1) {		      \
      st.table[slot].key = k;			      \
      st.table[slot].value = v;			      \
      st.keys++;				      \
      rv= 1; }					      \
    else if(st.table[slot].key == k) {		      \
      st.table[slot].value = v;			      \
      rv = 1; }					      \
    else if (st.table[slot].key != k) {		      \
      rv = -1; }				      \
    else { rv = -1; } }

#define o_get(st,rv,k) { int slot = k % HTCAPACITY; \
    if(st.table[slot].key != k) rv = -1;	    \
    else rv = st.table[slot].value; }

#define o_rm(st,rv,k) { int slot = k % HTCAPACITY;	\
  if(st.table[slot].key == k) {				\
    st.table[slot].key = -1; st.table[slot].value = -1; rv = 1; }	\
  else { rv = -1; } }

#define o_isempty(st,rv) { if(st.keys==0) rv = 1; else rv = 0; }

