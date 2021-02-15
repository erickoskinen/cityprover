#include <stdlib.h>

#define o_new(res) {                            \
    res = malloc(sizeof(struct state_t));       \
    res->front = 0;                             \
    res->rear = -1;                             \
    res->size = 0;                              \
  }

#define o_enq(st,rv,v) {                                      \
      if(st.size == MAXQUEUE) { rv = 0;   }                        \
      else if(st.rear == MAXQUEUE - 1) { rv = 0;  }                    \
      else {                                                            \
            st.size++; rv = 1;                                       \
            st.a[st.rear+1] = v;                                  \
            st.rear = st.rear + 1;                                \
      } }

#define o_deq(st,rv) {                                         \
   if(st.size==0) { rv = -1;  }                              \
   else { int r = st.a[st.front];                                 \
          st.front = (st.front + 1);                              \
          st.size--;                                                 \
          rv = r; } }

#define o_isempty(st,rv) { \
    rv = ( st.size==0 ? 1 : 0); }
