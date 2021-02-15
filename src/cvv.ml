(* ****************************************************************** *)
(* CityProver                                                         *)
(* Eric Koskinen                                                      *)
(* (c) 2019--2021                                                     *)
(* ****************************************************************** *)

type postcond = PC of (string -> string)

type arg =
 | UArg | IArg of string | IIArg of string * string

type rv = IRV

type meth = | Method of string * arg * rv

type verifiers = Vcpa | Vult

type oestage = | StageA | StageB

type phase = | RetVal | ObsEq of string * oestage | NonCom of string

(* ****************************************************************** *)

let string_of_phase p = match p with
  | RetVal -> "rv" | ObsEq(_,StageA) -> "oeA" | ObsEq(_,StageB) -> "oeB" | _ -> "unknown"


let string_of_method m =
  match m with
  | Method(s,UArg,IRV) -> s ^ "()/int"
  | Method(s,IArg(x),IRV) -> s ^ "(int)/int"
  | Method(s,IIArg(x1,x2),IRV) -> s ^ "(int,int)/int"

(* ****************************************************************** *)

let callm m st_var rv_var =
  match m with
  | Method(s,UArg,IRV)         -> "o_" ^ s ^ "(" ^ st_var ^ "," ^ rv_var ^ ")"
  | Method(s,IArg(x1),IRV)     -> "o_" ^ s ^ "(" ^ st_var ^ "," ^ rv_var ^ "," ^ x1 ^ ")"
  | Method(s,IIArg(x1,x2),IRV) -> "o_" ^ s ^ "(" ^ st_var ^ "," ^ rv_var ^ "," ^ x1 ^ "," ^ x2 ^ ")"

let callmnondet m st_var rv_var =
  match m with 
  | Method(s,UArg,IRV) -> callm m st_var rv_var
  | Method(s,IArg(x1),IRV) -> callm (Method(s,IArg("random()"),IRV)) st_var rv_var
  | Method(s,IIArg(x1,x2),IRV) -> callm (Method(s,IIArg("random()","random()"),IRV)) st_var rv_var


let rec mk_reachable_ms mlist icase =
   match mlist with
   | [] -> "   default: break;"
   | (m :: r) ->
      let cs = (string_of_int icase) in
      ("    case " ^ cs ^ ": " ^ (callmnondet m "s1" ("dum_"^cs)) ^ "; break;\n"
       ^ (mk_reachable_ms r (icase+1)))

let rec mk_reachable_dums mlist icase =
   match mlist with
   | [] -> ""
   | (m :: r) -> "  int dum_" ^ (string_of_int icase) ^ "; " ^ (mk_reachable_dums r (icase+1))

let rec mk_obseq_ms gotoerror mlist icase asrtoer =
   match mlist with
   | [] -> "   default: break;"
   | (m :: r) ->
      let cs = (string_of_int icase) in
      ("    case " ^ cs ^ ": \n"
     ^ "      " ^ (callm m "s1" "rv1") ^ ";\n"
     ^ "      " ^ (callm m "s2" "rv2") ^ ";\n"
     ^ "      " ^ asrtoer ^ "\n"
     ^ "      if(rv1 != rv2) "^gotoerror^"\n"
     ^ "      break;\n"
       ^ (mk_obseq_ms gotoerror r (icase+1) asrtoer))

exception BExn of string

type iqueue = { assume_code: string; assert_code: string; s1_post: string; s2_post: string }

type oer =
 | Itrue
 | Isimp of string
 | Iconj of string list
 | Idisj of string list
 (* Ultimate sometimes struggles with post-conditions being too far beyond the end of the method. Ipieces lets us put some of the (non)relational aspects of Ibeta directly into the method implementation. See Queue2 *)
 | Ibeta (* ob eq relation will be in source code file *)

let code_of_oer i : string = (* return a list of assume args *)
  match i with
  | Itrue -> "1==1"
  | Isimp(s) -> s
  | Iconj(sl) -> "("^(String.concat ") && (" sl)^")"
  | Idisj(sl) -> "("^(String.concat ") || (" sl)^")"
  | _ -> raise (BExn "code_of_oer")

(*                      m-f-rv    mem      [rd;wr]     rd     wr    varphi   obseq  *)
type bench = | Bench of string * string * meth list * meth * meth * string * oer

let all_bench() =
  let iarg = IArg("rho1") in
  let iiarg = IIArg("rho1","rho2") in
  let rd = Method("read",UArg,IRV) in
  let wr = Method("write",iarg,IRV) in
  let incr = Method("incr",UArg,IRV) in
  let decr = Method("decr",UArg,IRV) in
  let cclear = Method("clear",UArg,IRV) in
  let isz = Method("isz",UArg,IRV) in
  let ss3add = Method("add", iarg, IRV) in
  let ss3isin = Method("isin", iarg, IRV) in
  let ss3sz = Method("getsize", UArg, IRV) in
  let ss3clear = Method("clear", UArg, IRV) in
  let push = Method("push",iarg,IRV) in
  let pop = Method("pop",UArg,IRV) in
  let enq = Method("enq",iarg,IRV) in
  let deq = Method("deq",UArg,IRV) in
  let qempty = Method("isempty",UArg,IRV) in
  let isempty = Method("isempty",UArg,IRV) in

  let q2enq = Method("enq",iarg,IRV) in
  let q2deq = Method("deq",UArg,IRV) in
  let q2qempty = Method("isempty",UArg,IRV) in

  let put = Method("put",iiarg,IRV) in
  let get = Method("get",iarg,IRV) in
  let hempty = Method("isempty",UArg,IRV) in
  let hrm = Method("rm",iarg,IRV) in


  let iMem = Isimp("s1.x == s2.x") in
  let iAccumulator = Isimp("s1.x == s2.x") in
  let iCounter = Isimp("s1.x == s2.x") in
  let iCounterBad = Isimp("((s1.x > 0 && s2.x > 0) || (s1.x == 0 && s2.x == 0))") in
  let iSimpleSet = Isimp("( (s1.sz == s2.sz)  )"
        ^" && ( (s1.a == s2.a && s1.b == s2.b) || (s1.a == s2.b && s1.b == s2.a) )") in

  (* Ultimate and CPA checker don't support quantified invariants, so we have to do this manually :-/ *)
  let iStack = Isimp(" ( (s1.top == s2.top) && ("
              ^   " (s1.top < 0 || s1.a[0] == s2.a[0]) && "
              ^   " (s1.top < 1 || s1.a[1] == s2.a[1]) && "
              ^   " (s1.top < 2 || s1.a[2] == s2.a[2]) && "
              ^   " (s1.top < 3 || s1.a[3] == s2.a[3]) && "
              ^   " (s1.top < 4 || s1.a[4] == s2.a[4]) )) ") in

  (* ******************************************************************** *)
  [
    Bench("m-read-write-phi1-s",      "mem",[rd;wr],rd,wr,"s1.x == rho_y_1",iMem);
    Bench("m-read-write-true-frv",    "mem",[rd;wr],rd,wr,"1==1",iMem); 
    Bench("m-write-write-phi1-s",     "mem",[rd;wr],wr,wr,"rho_y_1 == rho_x_1",iMem);
    Bench("m-write-write-true-foeA",  "mem",[rd;wr],wr,wr,"1==1",iMem);
    Bench("m-read-read-true-s",       "mem",[rd;wr],rd,rd,"1==1",iMem);

    (* Accumulator - with another *)
    Bench("ac-decr-isz-phi1-s",   "accumulator",[incr;decr;isz],decr,isz,"s1.x > 1",iAccumulator);
    Bench("ac-decr-isz-true-frv", "accumulator",[incr;decr;isz],decr,isz,"1==1",iAccumulator);
    Bench("ac-decr-incr-phi1-s",  "accumulator",[incr;decr;isz],decr,incr,"s1.x > 1",iAccumulator);
    Bench("ac-decr-incr-true-s",  "accumulator",[incr;decr;isz],decr,incr,"1==1",iAccumulator);
    Bench("ac-incr-isz-phi1-s",   "accumulator",[incr;decr;isz],incr,isz,"s1.x > 1",iAccumulator);
    Bench("ac-incr-isz-true-frv", "accumulator",[incr;decr;isz],incr,isz,"1==1",iAccumulator);

    (* Accumulator - with self *)
    Bench("ac-incr-incr-true-s", "accumulator",[incr;decr;isz],incr,incr,"1==1",iAccumulator);
    Bench("ac-decr-decr-true-s", "accumulator",[incr;decr;isz],decr,decr,"1==1",iAccumulator);
    Bench("ac-decr-decr-phi1-s", "accumulator",[incr;decr;isz],decr,decr,"s1.x > 1",iAccumulator);
    Bench("ac-isz-isz-true-s","accumulator",[incr;decr;isz],isz,isz,"1==1",iAccumulator);

    (* Counter - like accumulator, but can't go below zero *)
    Bench("c-decr-decr-true-frv",  "counter",[incr;decr;isz;cclear],decr,decr,"1==1",iCounter);
    Bench("c-decr-decr-phi1-s",    "counter",[incr;decr;isz;cclear],decr,decr,"s1.x >= 2",iCounter);
    Bench("c-decr-incr-true-frv",  "counter",[incr;decr;isz;cclear],decr,incr,"1==1",iCounter);
    Bench("c-decr-incr-phi1-s",    "counter",[incr;decr;isz;cclear],decr,incr,"s1.x >= 1",iCounter);
    Bench("c-incr-isz-true-frv",   "counter",[incr;decr;isz;cclear],incr,isz,"1==1",iCounter);
    Bench("c-incr-isz-true-s",     "counter",[incr;decr;isz;cclear],incr,isz,"s1.x > 0",iCounter);
    Bench("c-incr-isz-true-foeB",  "counter",  [incr;decr;isz;cclear],incr,isz,"s1.x > 0",iCounterBad);
    Bench("c-incr-cclear-true-foeA","counter",[incr;decr;isz;cclear],incr,cclear,"1==1",iCounter); (* never commute *)



    (* Queue 2 *)
    Bench("q2-enq-enq-phi2-s","queue2",[q2enq;q2deq;q2qempty],q2enq,q2enq,"rho_x_1 == rho_y_1 && s1.size < 4",Ibeta);
    Bench("q2-enq-enq-true-foeB","queue2",[q2enq;q2deq;q2qempty],q2enq,q2enq,"1==1",Ibeta);
    Bench("q2-deq-deq-true-frv","queue2",[q2enq;q2deq;q2qempty],q2deq,q2deq,"1==1",Ibeta);
    Bench("q2-deq-deq-phi1-s","queue2",[q2enq;q2deq;q2qempty],q2deq,q2deq,"s1.size == 0",Ibeta);

    Bench("q2-enq-enq-true-frv","queue2",[enq;deq;qempty],enq,enq,"1==1",Ibeta);
    Bench("q2-enq-enq-phi1-frv",  "queue2",[enq;deq;qempty],enq,enq,"rho_x_1 == rho_y_1",Ibeta);
    Bench("q2-qempty-qempty-true-s","queue2",[enq;deq;qempty],qempty,qempty,"1==1",Ibeta);
    Bench("q2-enq-deq-phi1-s",  "queue2",[enq;deq;qempty],enq,deq,"s1.size == 1 && rho_x_1 == s1.a[s1.front]",Ibeta);
    Bench("q2-enq-deq-true-frv","queue2",[enq;deq;qempty],enq,deq,"1==1",Ibeta);
    Bench("q2-enq-qempty-phi1-s",  "queue2",[enq;deq;qempty],enq,qempty,"s1.size > 0",Ibeta);
    Bench("q2-enq-qempty-true-frv","queue2",[enq;deq;qempty],enq,qempty,"true",Ibeta);
    Bench("q2-deq-qempty-phi1-s",  "queue2",[enq;deq;qempty],deq,qempty,"s1.size == 0",Ibeta);
    Bench("q2-deq-qempty-true-frv","queue2",[enq;deq;qempty],deq,qempty,"true",Ibeta);



    (* ArrayStack *)
    Bench("as-push-pop-phi1-s",    "stack",[push;pop;isempty],push,pop,"s1.a[s1.top] == rho_x_1 && s1.top > 1 && s1.top < 5-1",iStack);
    Bench("as-push-pop-true-frv",  "stack",[push;pop;isempty],push,pop,"1==1",iStack);
    Bench("as-push-push-true-frv","stack",[push;pop;isempty],push,push,"1==1",iStack);
    Bench("as-push-push-phi1-foeA","stack",[push;pop;isempty],push,push,"s1.top < 3",iStack);
    Bench("as-push-push-phi2-frv",   "stack",[push;pop;isempty],push,push,"rho_x_1 == rho_y_1",iStack);
    Bench("as-push-push-phi3-s",   "stack",[push;pop;isempty],push,push,"rho_x_1 == rho_y_1 && s1.top < 3",iStack);
    Bench("as-pop-pop-phi1-s",     "stack",[push;pop;isempty],pop,pop,"s1.top == -1",iStack);
    Bench("as-pop-pop-true-frv",   "stack",[push;pop;isempty],pop,pop,"1==1",iStack);

    
    (* HashTable *)
    Bench("h-put-put-phi1-frv", "hashtable",[put;get;hempty;hrm],put,put,"rho_x_1 != rho_y_1",Ibeta);
    Bench("h-put-put-phi2-frv", "hashtable",[put;get;hempty;hrm],put,put,"rho_x_1 != rho_y_1 && s1.table[rho_x_1%HTCAPACITY].key == -1 && s1.table[rho_y_1%HTCAPACITY].key == -1",Ibeta);
    Bench("h-put-put-phi3-s",   "hashtable",[put;get;hempty;hrm],put,put,"rho_x_1 != rho_y_1 && rho_x_1%HTCAPACITY != rho_y_1%HTCAPACITY && s1.table[rho_x_1%HTCAPACITY].key == -1 && s1.table[rho_y_1%HTCAPACITY].key == -1",Ibeta);
    Bench("h-put-put-true-frv", "hashtable",[put;get;hempty;hrm],put,put,"1==1",Ibeta);
    Bench("h-get-get-phi1-s",   "hashtable",[put;get;hempty;hrm],get,get,"s1.keys == 0",Ibeta);
    Bench("h-get-get-true-s",   "hashtable",[put;get;hempty;hrm],get,get,"1 == 1",Ibeta);
    Bench("h-get-put-phi1-s",   "hashtable",[put;get;hempty;hrm],get,put,"rho_x_1 != rho_y_1",Ibeta);
    Bench("h-get-put-true-frv", "hashtable",[put;get;hempty;hrm],get,put,"1==1",Ibeta);

    (* SimpleSet3 - read only *)
    Bench("ss3-isin-isin-true-s","simpleset",[ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3isin,"1==1",iSimpleSet);
    (* SimpleSet3 - isin vs ___ *)
    Bench("ss3-isin-add-phi1-s",    "simpleset", [ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3add,"rho_x_1 != rho_y_1",iSimpleSet);
    Bench("ss3-isin-add-true-frv",  "simpleset", [ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3add,"1==1",iSimpleSet);
    Bench("ss3-isin-clear-true-frv","simpleset", [ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3clear,"1==1",iSimpleSet);
    Bench("ss3-isin-clear-phi1-frv",  "simpleset", [ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3clear,"rho_x_1 != rho_y_1",iSimpleSet);
    Bench("ss3-isin-clear-phi2-frv",  "simpleset", [ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3clear,"s1.a != rho_x_1 && s1.b != rho_y_1",iSimpleSet);
    Bench("ss3-isin-clear-phi3-s",  "simpleset", [ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3clear,"s1.a != rho_x_1 && s1.b != rho_x_1",iSimpleSet);
    Bench("ss3-isin-norm-true-s",   "simpleset", [ss3add;ss3isin;ss3clear;ss3sz],ss3isin,ss3sz,"1==1",iSimpleSet)


  ]

let s1_init_state s = 
  match s with
  | "stack" ->    "   s1.top = -1;\n"
  | "ss" ->       "  s1.a=-1; s1.b=-1;\n"
  | "ssnf" ->     "  s1.a=-1; s1.b=-1;\n"
  | "simpleset" ->      "  s1.a=-1; s1.b=-1; s1.sz = 0;\n"
  | "accumulator" ->  "  s1.x = 0;\n"
  | "counter"     ->  "  s1.x = 0;\n"
  | "mem"     ->  "  s1.x = 0;\n"
  | "queue2"  ->  "  s1.front = 0;\n  s1.rear=-1;\n  s1.size=0;\n"
  | "hashtable"   ->  "  s1.keys = 0;\n"
  | _ -> raise (BExn ("no alloc post for :"^s))

let assume_state_space s = 
  match s with
  | "stack" ->    ["s1.top >= -1"; "s1.top < 5"; "s2.top >= -1"; "s2.top < 5"]
  | "ssnf" ->     ["s1.a >= -1"; "s1.b >= -1"; "s2.a >= -1"; "s2.b >= -1"]
  | "simpleset"  ->     ["s1.a >= -1"; "s1.b >= -1"; "s1.sz >= 0"; "s2.a >= -1"; "s2.b >= -1"; "s2.sz >= 0"]
  | "accumulator" ->  []
  | "counter" ->  ["s1.x >= 0"; "s2.x >= 0"]
  | "mem"     ->  []
  | "queue2" 
  | "queue"   ->  ["s1.front >= 0"; "s1.front <= MAXQUEUE-1"; "s1.rear >= 0"; "s1.rear <= MAXQUEUE-1"; "s1.size >= 0"; "s1.size < MAXQUEUE"; "s2.front >= 0"; "s2.front <= MAXQUEUE-1"; "s2.rear >= 0"; "s2.rear <= MAXQUEUE-1"; "s2.size >= 0"; "s2.size < MAXQUEUE"]
  | "hashtable" -> ["s1.keys >= 0"; "s2.keys >= 0"]
  | _ -> raise (BExn ("no assume_state_space for :"^s))

let clone_post s =
match s with
  | "stack" ->  ( "  s2.top = s1.top;\n"
                 ^"   s2.a[0] = s1.a[0];\n"
                 ^"   s2.a[1] = s1.a[1];\n"
                 ^"   s2.a[2] = s1.a[2];\n"
                 ^"   s2.a[3] = s1.a[3];\n"
                 ^"   s2.a[4] = s1.a[4];\n")
  | "loop" ->  ( "  s2.top = s1.top;\n"
                 ^"   s2.a[0] = s1.a[0];\n"
                 ^"   s2.a[1] = s1.a[1];\n"
                 ^"   s2.a[2] = s1.a[2];\n"
                 ^"   s2.a[3] = s1.a[3];\n"
                 ^"   s2.a[4] = s1.a[4];\n")
  | "hashtable" ->  ( "  s2.keys = s1.keys;\n"
                 ^"   s2.table[0].key = s1.table[0].key;\n"
                 ^"   s2.table[1].key = s1.table[1].key;\n"
                 ^"   s2.table[2].key = s1.table[2].key;\n"
                 ^"   s2.table[3].key = s1.table[3].key;\n"
                 ^"   s2.table[4].key = s1.table[4].key;\n"
                 ^"   s2.table[5].key = s1.table[5].key;\n"
                 ^"   s2.table[6].key = s1.table[6].key;\n"
                 ^"   s2.table[7].key = s1.table[7].key;\n"
                 ^"   s2.table[8].key = s1.table[8].key;\n"
                 ^"   s2.table[9].key = s1.table[9].key;\n"
                 ^"   s2.table[10].key = s1.table[10].key;\n"
                 ^"   s2.table[0].value = s1.table[0].value;\n"
                 ^"   s2.table[1].value = s1.table[1].value;\n"
                 ^"   s2.table[2].value = s1.table[2].value;\n"
                 ^"   s2.table[3].value = s1.table[3].value;\n"
                 ^"   s2.table[4].value = s1.table[4].value;\n"
                 ^"   s2.table[5].value = s1.table[5].value;\n"
                 ^"   s2.table[6].value = s1.table[6].value;\n"
                 ^"   s2.table[7].value = s1.table[7].value;\n"
                 ^"   s2.table[8].value = s1.table[8].value;\n"
                 ^"   s2.table[9].value = s1.table[9].value;\n"
                 ^"   s2.table[10].value = s1.table[10].value;\n")
  | "queue2"
  | "queue" ->  ( "  s2.front = s1.front;\n"
                 ^"  s2.rear = s1.rear;\n"
                 ^"  s2.size = s1.size;\n"
                 ^"   s2.a[0] = s1.a[0];\n"
                 ^"   s2.a[1] = s1.a[1];\n"
                 ^"   s2.a[2] = s1.a[2];\n"
                 ^"   s2.a[3] = s1.a[3];\n"
                 ^"   s2.a[4] = s1.a[4];\n")
  | "simpleset" ->     "  s2.a = s1.a; s2.b=s1.b; s2.sz=s1.sz;\n"
  | "ssnf" ->   "  s2.a = s1.a; s2.b=s1.b;\n"
  | "accumulator" -> " s2.x = s1.x;\n"
  | "counter" -> " s2.x = s1.x;\n"
  | "mem"     -> " s2.x = s1.x;\n"
  | _ -> raise (BExn ("no clone post for :"^s))

let exact_eq s =
  let sl = (match s with
  | "stack" ->
     "s2.top == s1.top" ::
      (List.map (fun i -> (Printf.sprintf "s2.a[%d] == s1.a[%d]" i i))
                [0;1;2;3;4])
  | "hashtable" ->
     "s2.keys == s1.keys" ::
      (List.map (fun i -> (Printf.sprintf "s2.table[%d] == s1.table[%d]" i i))
                [0;1;2;3;4;5;6;7;8;9;10])
  | "queue" ->
      ["s2.front == s1.front"; "s2.rear == s1.rear"; "s2.size == s1.size"]
      @
      (List.map (fun i -> (Printf.sprintf "s2.a[%d] == s1.a[%d]" i i))
                [0;1;2;3;4])
  | "accumulator" -> ["s2.x == s1.x"]
  | "counter" -> [" s2.x == s1.x"]
  | "mem"     -> [" s2.x == s1.x"]
  | "list"    -> " s2.end == s1.end" ::
     (List.map (fun i -> (Printf.sprintf "s2.list[%d] == s1.list[%d]" i i))
                [0;1;2;3;4])
  | _ -> raise (BExn ("no exact_eq for :"^s))
  ) in
  String.concat " && " (List.map (fun s -> "("^s^")") sl)

let tex_of_b b = match b with
  | Bench(nm,adt,mlist,Method(m,_,_),Method(n,_,_),varphi,obseq) ->
     "$\\MM{" ^ m ^ "}{" ^ n ^ "}$"
let tex_of_varphi b = match b with
  | Bench(nm,adt,mlist,m,n,varphi,obseq) -> varphi

let obseq_of_b b =
  match b with 
    | Bench(n,_,_,_,_,_,dinv) -> dinv

let get_benchmark bname : bench =
  let rec _scan bs =
    match bs with | [] -> raise (BExn ("no bench:"^bname))
    | (Bench(n,_,_,_,_,_,_) :: r) ->
       if String.compare bname n == 0 then (List.hd bs)
       else _scan r
  in
  _scan (all_bench())

let benchlist bspec =
  String.concat "," (List.map (fun b ->
     match b with | Bench(n,_,_,_,_,_,_) -> n
  ) (all_bench()))

let setargs m xy =
   match m with
   | Method(name,UArg,IRV)         -> Method(name,UArg,IRV)
   | Method(name,IArg(x1),IRV)     -> Method(name,IArg("rho_"^xy^"_1"),IRV)
   | Method(name,IIArg(x1,x2),IRV) -> Method(name,IIArg("rho_"^xy^"_1","rho_"^xy^"_1"),IRV)

let allocate_vars() = (String.concat "\n  " [
    "  int rvm, rvn, rvmn, rvnm;";
    "  int rho_x_1 = random(); __VERIFIER_assume(rho_x_1 > 0);";
    "  int rho_x_2 = random(); __VERIFIER_assume(rho_x_2 > 0);";
    "  int rho_y_1 = random(); __VERIFIER_assume(rho_y_1 > 0);";
    "  struct state_t s1;";
    "  struct state_t s2;\n" ])

let gotoerror v = "__VERIFIER_error();"
(*match v with | Vcpa -> "goto ERROR;" | Vult -> "__VERIFIER_error();"*)

let assume_oer i =
  let mkasm (s : string) = ("assume("^s^");") in
  match i with
  | Ibeta -> "assume_ibeta()"
  | Iconj(fs) -> String.concat "\n   " (List.map mkasm fs)
  | _ -> mkasm (code_of_oer i)

let assert_oer ver i =
   let mkif s = "    if (!( " ^ s ^ " )) "^(gotoerror ver) in
   match i with
   | Ibeta -> "assert_ibeta()"
   | Iconj(fs) -> String.concat "\n   " (List.map mkif fs)
        (*"    if (!( " ^ phi ^ " )) "^(gotoerror ver)) fs)*)
   | _ -> mkif (code_of_oer i)

let mk_reachable_naive verifier b =
   match b with
   | Bench(bname,nm,mlist,mm,nn,varphi,_) ->
   (* print_endline "#include \"common.h\"";
    * print_endline "#include \"mem.h\""; *)
   let m = setargs mm "x" in
   let n = setargs nn "y" in
   print_endline ("// bench: " ^ bname);
   print_endline ("// phase: rv");
   print_endline "int main() {";
   print_endline (allocate_vars());
   print_endline "  // ******** NAIVE ENCODING ******* ";
   print_endline "  // post-condition of clone:";
   print_endline (clone_post nm);
   print_endline ("  __VERIFIER_assume( " ^ varphi ^ " );");
   print_endline ("  "^(callm m "s1" "rvm") ^ ";");
   print_endline ("  "^(callm n "s1" "rvmn") ^ ";");
   print_endline ("  "^(callm n "s2" "rvn") ^ ";");
   print_endline ("  "^(callm m "s2" "rvnm") ^ ";");
   print_endline ("  while(1>0) { // force compute invariant");
   print_endline ("    if(rvm != rvnm) "^(gotoerror verifier));
   print_endline ("    if(rvn != rvmn) "^(gotoerror verifier));
   print_endline ("    if(!("^(exact_eq nm)^")) "^(gotoerror verifier));
   print_endline ("  }");
   print_endline (match verifier with | Vult -> "  return 0;" | Vcpa -> "  return 0;\n ERROR:\n  return (-1);");
   print_endline ("}")


let mk_reachable_oneshot verifier b inv =
   match b with
   | Bench(bname,nm,mlist,mm,nn,varphi,_) ->
   (* print_endline "#include \"common.h\"";
    * print_endline "#include \"mem.h\""; *)
   let m = setargs mm "x" in
   let n = setargs nn "y" in
   print_endline ("// bench: " ^ bname);
   print_endline ("// phase: oneshot");
   print_endline "int main() {";
   print_endline (allocate_vars());
   print_endline "  // ******** ONESHOT ENCODING ******* ";
   print_endline (s1_init_state nm);
   print_endline (mk_reachable_dums mlist 0);
   print_endline "  while(random()) { switch(random()) {";
   print_endline (mk_reachable_ms mlist 0);
   print_endline "  } }";
   print_endline "  // post-condition of clone:";
   print_endline (clone_post nm);
   print_endline ("  __VERIFIER_assume( " ^ varphi ^ " );");
   print_endline ("  "^(callm m "s1" "rvm") ^ ";");
   print_endline ("  "^(callm n "s1" "rvmn") ^ ";");
   print_endline ("  "^(callm n "s2" "rvn") ^ ";");
   print_endline ("  "^(callm m "s2" "rvnm") ^ ";");
   print_endline ("  if(rvm != rvnm) "^(gotoerror verifier));
   print_endline ("  if(rvn != rvmn) "^(gotoerror verifier));
   print_endline "  // ******** ONESHOT OBS EQ ******* ";
   print_endline "  while(random()) {";
   print_endline (assert_oer verifier inv);
   print_endline "    int rv1, rv2;";
   print_endline "    int rho1 = random(); __VERIFIER_assume(rho1 >= 0);";
   print_endline "    int rho2 = random(); __VERIFIER_assume(rho2 >= 0);";
   print_endline "    switch(random()) {";
   print_endline (mk_obseq_ms (gotoerror verifier) mlist 0 "");
   print_endline "    }";
   print_endline "  }";
   print_endline (match verifier with | Vult -> "  return 0;" | Vcpa -> "  return 0;\n ERROR:\n  return (-1);");
   print_endline ("}")

type phase2 = RV | OEA

let string_of_phase p = match p with RV -> "rv" | OEA -> "oeA"

let mk_reachable verifier b (p:phase2) inv = 
   match b with
   | Bench(bname,nm,mlist,mm,nn,varphi,_) ->
     let m = setargs mm "x" in
     let n = setargs nn "y" in
     print_endline "  // ******** COMMUTATIVITY OBSERVATIONAL EQUIVALENCE STAGE A ******* ";
     print_endline ("// bench: " ^ bname);
     print_endline ("// phase: " ^ (string_of_phase p));
     print_endline "int main() {";
     print_endline (allocate_vars());
     print_endline " // (for oeA/oeB, initial state is unconstrained, but they are concretely equal:)";
     print_endline (s1_init_state nm);
     print_endline (mk_reachable_dums mlist 0);
     print_endline "  while(random()) { switch(random()) {";
     print_endline (mk_reachable_ms mlist 0);
     print_endline "  } }";
     print_endline "  // post-condition of clone:";
     print_endline (clone_post nm);
     print_endline (clone_post nm);
     print_endline ("  __VERIFIER_assume( " ^ varphi ^ " );");
     print_endline ("  "^(callm m "s1" "rvm") ^ ";");
     print_endline ("  "^(callm n "s1" "rvmn") ^ ";");
     print_endline ("  "^(callm n "s2" "rvn") ^ ";");
     print_endline ("  "^(callm m "s2" "rvnm") ^ ";");
     (match p with
     | RV -> begin 
         print_endline "  // ******** COMMUTATIVITY RET VAL ENCODING ******* ";
         print_endline ("    if(rvm != rvnm) "^(gotoerror verifier));
         print_endline ("    if(rvn != rvmn) "^(gotoerror verifier)); end
     | OEA -> begin
         print_endline "  // ******** COMMUTATIVITY OBSERVATIONAL EQUIVALENCE STAGE A ******* ";
         print_endline ("  __VERIFIER_assume( rvm == rvmn );");
         print_endline ("  __VERIFIER_assume( rvn == rvnm );");
         print_endline (assert_oer verifier inv); end);
     print_endline (match verifier with | Vult -> "  return 0;" | Vcpa -> "  return 0;\n ERROR:\n  return (-1);");
     print_endline ("}")


let mk_reachable_oeB verifier b inv =
   match b with
   | Bench(bname,nm,mlist,mm,nn,varphi,dinv) ->
   (* print_endline "#include \"common.h\"";
    * print_endline "#include \"mem.h\""; *)
   print_endline "  // ******** COMMUTATIVITY OBSERVATIONAL EQUIVALENCE STAGE B ******* ";
   print_endline ("// bench: " ^ bname);
   print_endline ("// phase: oeB ");
   print_endline "int main() {";
   print_endline "  struct state_t s1;\n";
   print_endline "  struct state_t s2;\n";
   print_endline "  // state space assumptions\n";
   List.iter (fun asm -> print_endline ("  __VERIFIER_assume("^asm^");\n")) (assume_state_space nm);
   print_endline ("  // assume the invariant because StageA proved it");
   print_endline (assume_oer inv);
   print_endline "  int rv1, rv2;";
   print_endline "    int rho1 = random(); __VERIFIER_assume(rho1 >= 0);";
   print_endline "    int rho2 = random(); __VERIFIER_assume(rho2 >= 0);";
   print_endline "    switch(random()) {";
   print_endline (mk_obseq_ms (gotoerror verifier) mlist 0 (assert_oer verifier inv));
   print_endline "  }";
   print_endline (match verifier with | Vult -> "  return 0;" | Vcpa -> "  return 0;\n ERROR:\n  return (-1);");
   print_endline ("}")



let parse_verifier vn = match vn with
  | "cpa" -> Vcpa | "ult" -> Vult | _ -> failwith "can't parse verifier name"

let _ =
  let verifier_name = Sys.argv.(1) in
  if String.compare verifier_name "--benches" = 0 && Array.length Sys.argv = 2 then
    print_endline (benchlist "")
  else if String.compare verifier_name "--benches" = 0 then
    print_endline (benchlist Sys.argv.(2))
  else if String.compare verifier_name "--methods" = 0 then
    print_endline (tex_of_b (get_benchmark Sys.argv.(2)))
  else if String.compare verifier_name "--varphi" = 0 then
    print_endline (tex_of_varphi (get_benchmark Sys.argv.(2)))
  else
    let verifier = parse_verifier verifier_name in
    let b = (get_benchmark Sys.argv.(2)) in
    print_endline ("// generating: " ^ Sys.argv.(1));
    match Sys.argv.(3) with
       | "naive" -> mk_reachable_naive verifier b
       | "oneshot" -> mk_reachable_oneshot verifier b (obseq_of_b b)
       | "rv"  -> mk_reachable verifier b RV (obseq_of_b b)
       | "oeA" -> mk_reachable verifier b OEA (obseq_of_b b)
       | "oeB" -> mk_reachable_oeB verifier b (obseq_of_b b)
       | _ -> raise (BExn "no such phase")
