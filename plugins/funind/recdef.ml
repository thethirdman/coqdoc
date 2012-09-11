(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2012     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

open Tacexpr
open Term
open Namegen
open Environ
open Declarations
open Entries
open Pp
open Names
open Libnames
open Globnames
open Nameops
open Errors
open Util
open Closure
open RedFlags
open Tacticals
open Typing
open Tacmach
open Tactics
open Nametab
open Decls
open Declare
open Decl_kinds
open Tacred
open Proof_type
open Vernacinterp
open Pfedit
open Topconstr
open Glob_term
open Pretyping
open Safe_typing
open Constrintern
open Hiddentac
open Misctypes
open Genredexpr

open Equality
open Auto
open Eauto

open Indfun_common



(* Ugly things which should not be here *)

let coq_base_constant s =
  Coqlib.gen_constant_in_modules "RecursiveDefinition"
    (Coqlib.init_modules @ [["Coq";"Arith";"Le"];["Coq";"Arith";"Lt"]]) s;;

let coq_constant s =
  Coqlib.gen_constant_in_modules "RecursiveDefinition"
    (Coqlib.init_modules @ Coqlib.arith_modules) s;;

let find_reference sl s =
    (locate (make_qualid(Names.make_dirpath
			   (List.map id_of_string (List.rev sl)))
	       (id_of_string s)));;


let (declare_fun : identifier -> logical_kind -> constr -> global_reference) =
  fun f_id kind value ->
    let ce = {const_entry_body = value;
              const_entry_secctx = None;
	      const_entry_type = None;
	      const_entry_opaque = false } in
      ConstRef(declare_constant f_id (DefinitionEntry ce, kind));;

let defined () = Lemmas.save_named false

let def_of_const t =
   match (kind_of_term t) with
    Const sp ->
      (try (match body_of_constant (Global.lookup_constant sp) with
             | Some c -> Declarations.force c
	     | _ -> assert false)
       with _ ->
	 anomaly ("Cannot find definition of constant "^
		    (string_of_id (id_of_label (con_label sp))))
      )
     |_ -> assert false

let type_of_const t =
   match (kind_of_term t) with
    Const sp -> Typeops.type_of_constant (Global.env()) sp
    |_ -> assert false


let constant sl s =
  constr_of_global
    (locate (make_qualid(Names.make_dirpath
			   (List.map id_of_string (List.rev sl)))
	       (id_of_string s)));;
let const_of_ref = function
    ConstRef kn -> kn
  | _ -> anomaly "ConstRef expected"


let nf_zeta env =
  Reductionops.clos_norm_flags  (Closure.RedFlags.mkflags [Closure.RedFlags.fZETA])
    env
    Evd.empty


let nf_betaiotazeta = (* Reductionops.local_strong Reductionops.whd_betaiotazeta  *)
  let clos_norm_flags flgs env sigma t =
    Closure.norm_val (Closure.create_clos_infos flgs env) (Closure.inject (Reductionops.nf_evar sigma t)) in
  clos_norm_flags Closure.betaiotazeta  Environ.empty_env Evd.empty






(* Generic values *)
let pf_get_new_ids idl g =
  let ids = pf_ids_of_hyps g in
  List.fold_right
    (fun id acc -> next_global_ident_away id (acc@ids)::acc)
    idl
    []

let compute_renamed_type gls c =
  rename_bound_vars_as_displayed (*no avoid*) [] (*no rels*) []
    (pf_type_of gls c)
let h'_id = id_of_string "h'"
let heq_id = id_of_string "Heq"
let teq_id = id_of_string "teq"
let ano_id = id_of_string "anonymous"
let x_id = id_of_string "x"
let k_id = id_of_string "k"
let v_id = id_of_string "v"
let def_id = id_of_string "def"
let p_id = id_of_string "p"
let rec_res_id = id_of_string "rec_res";;
let lt = function () -> (coq_base_constant "lt")
let le = function () -> (coq_base_constant "le")
let ex = function () -> (coq_base_constant "ex")
let nat = function () -> (coq_base_constant "nat")
let coq_sig = function () -> (coq_base_constant "sig")
let iter_ref () =  
  try find_reference ["Recdef"] "iter" 
  with Not_found -> error "module Recdef not loaded"
let iter = function () -> (constr_of_global (delayed_force iter_ref))
let eq = function () -> (coq_base_constant "eq")
let le_lt_SS = function () -> (constant ["Recdef"] "le_lt_SS")
let le_lt_n_Sm = function () -> (coq_base_constant "le_lt_n_Sm")
let le_trans = function () -> (coq_base_constant "le_trans")
let le_lt_trans = function () -> (coq_base_constant "le_lt_trans")
let lt_S_n = function () -> (coq_base_constant "lt_S_n")
let le_n = function () -> (coq_base_constant "le_n")
let coq_sig_ref = function () -> (find_reference ["Coq";"Init";"Specif"] "sig")
let coq_O = function () -> (coq_base_constant "O")
let coq_S = function () -> (coq_base_constant "S")
let gt_antirefl = function () -> (coq_constant "gt_irrefl")
let lt_n_O = function () -> (coq_base_constant "lt_n_O")
let lt_n_Sn = function () -> (coq_base_constant "lt_n_Sn")
let f_equal = function () -> (coq_constant "f_equal")
let well_founded_induction = function () -> (coq_constant "well_founded_induction")
let max_ref = function () -> (find_reference ["Recdef"] "max")
let max_constr = function () -> (constr_of_global (delayed_force max_ref))
let coq_conj = function () -> find_reference ["Coq";"Init";"Logic"] "conj"

let f_S t = mkApp(delayed_force coq_S, [|t|]);;
let make_refl_eq constructor type_of_t t  =
  mkApp(constructor,[|type_of_t;t|])

let rec n_x_id ids n =
  if n = 0 then []
  else let x = next_ident_away_in_goal x_id ids in
          x::n_x_id (x::ids) (n-1);;


let simpl_iter clause =
  reduce
    (Lazy
       {rBeta=true;rIota=true;rZeta= true; rDelta=false;
        rConst = [ EvalConstRef (const_of_ref (delayed_force iter_ref))]})
    clause

(* Others ugly things ... *)
let (value_f:constr list -> global_reference -> constr) =
  fun al fterm ->
    let d0 = Loc.ghost in
    let rev_x_id_l =
      (
	List.fold_left
	  (fun x_id_l _ ->
	     let x_id = next_ident_away_in_goal x_id x_id_l in
	     x_id::x_id_l
	  )
	  []
	  al
      )
    in
    let context = List.map
      (fun (x, c) -> Name x, None, c) (List.combine rev_x_id_l (List.rev al))
    in
    let env = Environ.push_rel_context context (Global.env ()) in
    let glob_body =
      GCases
	(d0,RegularStyle,None,
	 [GApp(d0, GRef(d0,fterm), List.rev_map (fun x_id -> GVar(d0, x_id)) rev_x_id_l),
	  (Anonymous,None)],
	 [d0, [v_id], [PatCstr(d0,(destIndRef
				     (delayed_force coq_sig_ref),1),
			       [PatVar(d0, Name v_id);
				PatVar(d0, Anonymous)],
			       Anonymous)],
	  GVar(d0,v_id)])
    in
    let body = understand Evd.empty env glob_body in
    it_mkLambda_or_LetIn body context

let (declare_f : identifier -> logical_kind -> constr list -> global_reference -> global_reference) =
  fun f_id kind input_type fterm_ref ->
    declare_fun f_id kind (value_f input_type fterm_ref);;



(* Debuging mechanism *)
let debug_queue = Stack.create ()

let rec print_debug_queue b e = 
  if  not (Stack.is_empty debug_queue) 
  then
    begin
      let lmsg,goal = Stack.pop debug_queue in 
      if b then 
	Pp.msg_debug (lmsg ++ (str " raised exception " ++ Errors.print e) ++ str " on goal " ++ goal)
      else
	begin
	  Pp.msg_debug (str " from " ++ lmsg ++ str " on goal " ++ goal);
	end;
      print_debug_queue false e;
    end

let observe strm =
  if do_observe ()
  then Pp.msg_debug strm
  else ()


let do_observe_tac s tac g = 
  let goal = Printer.pr_goal g in
  let lmsg = (str "recdef : ") ++ s in 
  observe (s++fnl());
  Stack.push (lmsg,goal) debug_queue;
  try 
    let v = tac g in 
    ignore(Stack.pop debug_queue);
    v
  with e -> 
    if not (Stack.is_empty debug_queue)
    then
      begin
	let e : exn = Cerrors.process_vernac_interp_error e in
	print_debug_queue true e
      end
    ; 
    raise e

let observe_tac s tac g =
  if do_observe ()
  then do_observe_tac s tac g
  else tac g

(* Conclusion tactics *)

(* The boolean value is_mes expresses that the termination is expressed
  using a measure function instead of a well-founded relation. *)
let tclUSER tac is_mes l g =
  let clear_tac =
    match l with
      | None -> h_clear false []
      | Some l -> tclMAP (fun id -> tclTRY (h_clear false [id])) (List.rev l)
  in
  tclTHENSEQ
    [
      clear_tac;
      if is_mes
      then tclTHENLIST
        [
	  unfold_in_concl [(Locus.AllOccurrences, evaluable_of_global_reference
            (delayed_force Indfun_common.ltof_ref))];
         tac
	 ]
      else tac
    ]
    g

let tclUSER_if_not_mes concl_tac is_mes names_to_suppress =
  if is_mes
  then tclCOMPLETE (h_simplest_apply (delayed_force well_founded_ltof))
  else tclUSER concl_tac is_mes names_to_suppress



  

(* Travelling term.
   Both definitions of [f_terminate] and [f_equation] use the same generic 
   travelling mechanism.
*)

(* [check_not_nested forbidden e] checks that [e] does not contains any variable 
   of [forbidden]
*)
let check_not_nested forbidden e =
  let rec check_not_nested e =  
    match kind_of_term e with 
      | Rel _ -> ()
      | Var x -> if List.mem x (forbidden) then error ("check_not_nested : failure "^string_of_id x)
      | Meta _ | Evar _ | Sort _ -> ()
      | Cast(e,_,t) -> check_not_nested e;check_not_nested t
      | Prod(_,t,b) -> check_not_nested t;check_not_nested b
      | Lambda(_,t,b) -> check_not_nested t;check_not_nested b
      | LetIn(_,v,t,b) -> check_not_nested t;check_not_nested b;check_not_nested v
      | App(f,l) -> check_not_nested f;Array.iter check_not_nested l
      | Const _ -> ()
      | Ind _ -> ()
      | Construct _ -> ()
      | Case(_,t,e,a) -> 
	check_not_nested t;check_not_nested e;Array.iter check_not_nested a
      | Fix _ -> error "check_not_nested : Fix"
      | CoFix _ -> error "check_not_nested : Fix"
  in
  try 
    check_not_nested e 
  with UserError(_,p) -> 
    errorlabstrm "_" (str "on expr : " ++ Printer.pr_lconstr e ++ str " " ++ p)

(* ['a info] contains the local information for travelling *)
type 'a infos = 
    { nb_arg : int; (* function number of arguments *)
      concl_tac : tactic; (* final tactic to finish proofs *)
      rec_arg_id : identifier; (*name of the declared recursive argument *)
      is_mes : bool; (* type of recursion *)
      ih : identifier; (* induction hypothesis name *)
      f_id : identifier;  (* function name *)
      f_constr : constr;  (* function term *)
      f_terminate : constr; (* termination proof term *)
      func : global_reference; (* functionnal reference *)
      info : 'a;
      is_main_branch : bool; (* on the main branch or on a matched expression *)
      is_final : bool; (* final first order term or not *)
      values_and_bounds : (identifier*identifier) list; 
      eqs : identifier list; 
      forbidden_ids : identifier list;
      acc_inv : constr lazy_t;
      acc_id : identifier;
      args_assoc : ((constr list)*constr) list;
    }


type ('a,'b) journey_info_tac = 
    'a -> (* the arguments of the constructor *)
    'b infos -> (* infos of the caller *)
    ('b infos -> tactic) -> (* the continuation tactic of the caller *)
    'b infos -> (* argument of the tactic *)
    tactic
       
(* journey_info : specifies the actions to do on the different term constructors during the travelling of the term
*)
type journey_info = 
    { letiN : ((name*constr*types*constr),constr) journey_info_tac;
      lambdA : ((name*types*constr),constr) journey_info_tac;
      casE : ((constr infos -> tactic) -> constr infos -> tactic) -> 
	((case_info * constr * constr * constr array),constr) journey_info_tac;
      otherS : (unit,constr) journey_info_tac;
      apP : (constr*(constr list),constr) journey_info_tac;
      app_reC : (constr*(constr list),constr) journey_info_tac;
      message : string
    }

	       

let rec add_vars forbidden e = 
  match kind_of_term e with 
    | Var x -> x::forbidden 
    | _ -> fold_constr add_vars forbidden e 


let treat_case forbid_new_ids to_intros finalize_tac nb_lam e infos : tactic = 
  fun g -> 
    let rev_context,b = decompose_lam_n nb_lam e in 
    let ids = List.fold_left (fun acc (na,_) -> 	
      let pre_id = 
	match na with 
	  | Name x -> x 
	  | Anonymous -> ano_id 
      in
      pre_id::acc
    ) [] rev_context in 
    let rev_ids = pf_get_new_ids (List.rev ids) g in 
    let new_b = substl (List.map mkVar rev_ids) b in 
    tclTHENSEQ 
      [
	h_intros (List.rev rev_ids);
	intro_using teq_id;
	onLastHypId (fun heq -> 
	  tclTHENSEQ[
	    thin to_intros;
	    h_intros to_intros;
	    (fun g' -> 
	      let ty_teq = pf_type_of g' (mkVar heq) in
	      let teq_lhs,teq_rhs =
		let _,args = try destApp ty_teq with _ -> assert false in
		args.(1),args.(2)
	      in
	      let new_b' = Termops.replace_term teq_lhs teq_rhs new_b in 
	      let new_infos = {
		infos with 
		  info = new_b';
		  eqs = heq::infos.eqs;
		  forbidden_ids = 
		  if forbid_new_ids 
		  then add_vars infos.forbidden_ids new_b'
		  else infos.forbidden_ids
	      } in 
	      finalize_tac new_infos g' 
	    )
	  ]
	)
      ] g

let rec travel_aux jinfo continuation_tac (expr_info:constr infos) =
  match kind_of_term expr_info.info with 
    | CoFix _ | Fix _ -> error "Function cannot treat local fixpoint or cofixpoint"
    | LetIn(na,b,t,e) -> 
      begin
	let new_continuation_tac = 
	  jinfo.letiN (na,b,t,e) expr_info continuation_tac
	in
	travel jinfo new_continuation_tac 
	  {expr_info with info = b; is_final=false}
      end
    | Rel _ -> anomaly "Free var in goal conclusion !"
    | Prod _ -> 
      begin
	try
	  check_not_nested (expr_info.f_id::expr_info.forbidden_ids) expr_info.info;
	  jinfo.otherS () expr_info continuation_tac expr_info
	with _ -> 
	  errorlabstrm "Recdef.travel" (str "the term " ++ Printer.pr_lconstr expr_info.info ++ str " can not contain a recursive call to " ++ pr_id expr_info.f_id)
      end
    | Lambda(n,t,b) -> 
      begin
	try
	  check_not_nested (expr_info.f_id::expr_info.forbidden_ids) expr_info.info;
	  jinfo.otherS () expr_info continuation_tac expr_info
	with _ -> 
	  errorlabstrm "Recdef.travel" (str "the term " ++ Printer.pr_lconstr expr_info.info ++ str " can not contain a recursive call to " ++ pr_id expr_info.f_id)
      end
    | Case(ci,t,a,l) -> 
      begin
	let continuation_tac_a = 
	  jinfo.casE 
	    (travel jinfo) (ci,t,a,l) 
	    expr_info continuation_tac in 
	travel 
	  jinfo continuation_tac_a 
	  {expr_info with info = a; is_main_branch = false; 
	    is_final = false}
      end
    | App _ -> 
      let f,args = decompose_app expr_info.info in 
      if eq_constr f (expr_info.f_constr) 
      then jinfo.app_reC (f,args) expr_info continuation_tac expr_info 
      else
      begin
	match kind_of_term f with 
	  | App _ -> assert false (* f is coming from a decompose_app *)
	  | Const _ | Construct _ | Rel _ | Evar _ | Meta _  | Ind _ 
	  | Sort _ | Prod _ | Var _ -> 
	    let new_infos = {expr_info with info=(f,args)} in 
	    let new_continuation_tac = 
	      jinfo.apP (f,args) expr_info continuation_tac in 
	    travel_args jinfo
	      expr_info.is_main_branch new_continuation_tac new_infos
	  | _ -> assert false 
      end
    | Cast(t,_,_) -> travel jinfo continuation_tac {expr_info with info=t}
    | Const _ | Var _ | Meta _ | Evar _ | Sort _ | Construct _ | Ind _ ->
      let new_continuation_tac = 
	jinfo.otherS () expr_info continuation_tac in
      new_continuation_tac expr_info
and travel_args jinfo is_final continuation_tac infos = 
  let (f_args',args) = infos.info in 
  match args with 
    | [] -> 
      continuation_tac {infos with info = f_args'; is_final = is_final}
    | arg::args' -> 
      let new_continuation_tac new_infos = 
	let new_arg = new_infos.info in 
	travel_args jinfo is_final
	  continuation_tac
	  {new_infos with info = (mkApp(f_args',[|new_arg|]),args')}
      in
      travel jinfo new_continuation_tac 
	{infos with info=arg;is_final=false} 
and travel jinfo continuation_tac expr_info = 
  observe_tac 
    (str jinfo.message ++ Printer.pr_lconstr expr_info.info) 
    (travel_aux jinfo continuation_tac expr_info)

(* Termination proof *) 

let rec prove_lt hyple g = 
  begin
    try 
      let (_,args) = decompose_app (pf_concl g) in 
      let x = try destVar (List.hd args) with _ -> assert false in 
      let z = try destVar (List.hd (List.tl args)) with _ -> assert false in 
      let h = 
	List.find (fun id -> 
	  let _,args' = decompose_app (pf_type_of g (mkVar id)) in  
	  try x = destVar (List.hd args')
	  with _ -> false
	) hyple 
      in 
      let y = 
	List.hd (List.tl (snd (decompose_app (pf_type_of g (mkVar h))))) in 
      tclTHENLIST[
	apply (mkApp(le_lt_trans (),[|mkVar x;y;mkVar z;mkVar h|]));
	observe_tac (str "prove_lt") (prove_lt hyple)
      ]       
    with Not_found -> 
      (
	(
	  tclTHENLIST[
	    apply (delayed_force lt_S_n);
	    (observe_tac (str "assumption: " ++ Printer.pr_goal g) (h_assumption))
	  ])
      )
  end
    g

let rec destruct_bounds_aux infos (bound,hyple,rechyps) lbounds g = 
  match lbounds with 
    | [] -> 
      let ids = pf_ids_of_hyps g in 
      let s_max = mkApp(delayed_force coq_S, [|bound|]) in 
      let k = next_ident_away_in_goal k_id ids in
      let ids = k::ids in
      let h' = next_ident_away_in_goal (h'_id) ids in
      let ids = h'::ids in
      let def = next_ident_away_in_goal def_id ids in
      tclTHENLIST[
	split (ImplicitBindings [s_max]);
	intro_then
	  (fun id -> 
	    observe_tac (str "destruct_bounds_aux") 
	      (tclTHENS (simplest_case (mkVar id))
		 [
		   tclTHENLIST[intro_using h_id;
			       simplest_elim(mkApp(delayed_force lt_n_O,[|s_max|]));
			       default_full_auto];
		   tclTHENLIST[
		     observe_tac (str "clearing k ") (clear [id]);
		     h_intros [k;h';def];
		     observe_tac (str "simple_iter") (simpl_iter Locusops.onConcl);
		     observe_tac (str "unfold functional")
		       (unfold_in_concl[(Locus.OnlyOccurrences [1],
					 evaluable_of_global_reference infos.func)]);
		     observe_tac (str "test" ) (
		       tclTHENLIST[
			 list_rewrite true
			   (List.fold_right 
			      (fun e acc -> (mkVar e,true)::acc)
			      infos.eqs
			      (List.map (fun e -> (e,true)) rechyps)
			   );
		      (* list_rewrite true *)
		      (*   (List.map (fun e -> (mkVar e,true)) infos.eqs) *)
		      (*   ; *)
			 
			 (observe_tac (str "finishing") 
			    (tclORELSE
			       h_reflexivity
			       (observe_tac (str "calling prove_lt") (prove_lt hyple))))])
		   ]
	      ]
	      ))
	  ] g
      | (_,v_bound)::l -> 
      tclTHENLIST[
	simplest_elim (mkVar v_bound);
	h_clear false [v_bound];
	tclDO 2 intro;
	onNthHypId 1 
	  (fun p_hyp -> 
	    (onNthHypId 2 
	       (fun p ->  
		 tclTHENLIST[
		   simplest_elim 
		     (mkApp(delayed_force max_constr, [| bound; mkVar p|]));
		   tclDO 3 intro;
		   onNLastHypsId 3 (fun lids -> 
		     match lids with
		       [hle2;hle1;pmax] -> 
			 destruct_bounds_aux infos 
			   ((mkVar pmax),
			    hle1::hle2::hyple,(mkVar p_hyp)::rechyps) 
			   l 
		       | _ -> assert false) ;
		 ]
	       )
	    )
	  )
      ] g

let destruct_bounds infos = 
  destruct_bounds_aux infos (delayed_force coq_O,[],[]) infos.values_and_bounds

let terminate_app f_and_args expr_info continuation_tac infos = 
    if expr_info.is_final && expr_info.is_main_branch 
    then 
      tclTHENLIST[
	continuation_tac infos;
	observe_tac (str "first split") 
	  (split (ImplicitBindings [infos.info]));
	observe_tac (str "destruct_bounds (1)") (destruct_bounds infos)
      ]
    else continuation_tac infos

let terminate_others _ expr_info continuation_tac infos = 
  if expr_info.is_final && expr_info.is_main_branch 
  then 
    tclTHENLIST[
	    continuation_tac infos;
      observe_tac (str "first split") 
	(split (ImplicitBindings [infos.info]));
      observe_tac (str "destruct_bounds") (destruct_bounds infos)
    ]
  else continuation_tac infos

let terminate_letin (na,b,t,e) expr_info continuation_tac info = 
  let new_e = subst1 info.info e in 
  let new_forbidden = 
    let forbid = 
      try 
	check_not_nested (expr_info.f_id::expr_info.forbidden_ids) b;
	true
      with _ -> false 
    in
    if forbid 
    then 
      match na with
	| Anonymous -> info.forbidden_ids
	| Name id -> id::info.forbidden_ids
    else info.forbidden_ids 
  in
  continuation_tac {info with info = new_e; forbidden_ids = new_forbidden} 



(* This is like the previous one except that it also rewrite on all
  hypotheses except the ones given in the first argument.  All the
  modified hypotheses are generalized in the process and should be
  introduced back later; the result is the pair of the tactic and the
  list of hypotheses that have been generalized and cleared. *)
let mkDestructEq :
  identifier list -> constr -> goal sigma -> tactic * identifier list =
  fun not_on_hyp expr g ->
  let hyps = pf_hyps g in
  let to_revert =
    Util.map_succeed
      (fun (id,_,t) ->
        if List.mem id not_on_hyp || not (Termops.occur_term expr t)
        then failwith "is_expr_context";
        id) hyps in
  let to_revert_constr = List.rev_map mkVar to_revert in
  let type_of_expr = pf_type_of g expr in
  let new_hyps = mkApp(Lazy.force refl_equal, [|type_of_expr; expr|])::
           to_revert_constr in
    tclTHENLIST
     [h_generalize new_hyps;
      (fun g2 ->
	change_in_concl None
	  (pattern_occs [Locus.AllOccurrencesBut [1], expr] (pf_env g2) Evd.empty (pf_concl g2)) g2);
      simplest_case expr], to_revert


let terminate_case next_step (ci,a,t,l) expr_info continuation_tac infos g =
  let b =
    try
      check_not_nested (expr_info.f_id::expr_info.forbidden_ids) a;
      false
    with _ ->
      true
  in
  let a' = infos.info in
  let new_info =
    {infos with
      info = mkCase(ci,t,a',l);
      is_main_branch = expr_info.is_main_branch;
      is_final = expr_info.is_final} in
  let destruct_tac,rev_to_thin_intro = 
    mkDestructEq [expr_info.rec_arg_id] a' g in 
  let to_thin_intro = List.rev rev_to_thin_intro in 
  observe_tac (str "treating case " ++ int (Array.length l) ++ spc () ++ Printer.pr_lconstr a') 
    (try
      (tclTHENS
	 destruct_tac
	 (list_map_i (fun i e -> observe_tac (str "do treat case") (treat_case b to_thin_intro (next_step continuation_tac) ci.ci_cstr_ndecls.(i) e new_info)) 0 (Array.to_list l)
	 ))
    with 
      | UserError("Refiner.thensn_tac3",_) 
      | UserError("Refiner.tclFAIL_s",_) ->
	(observe_tac (str "is computable " ++ Printer.pr_lconstr new_info.info) (next_step continuation_tac {new_info with info = nf_betaiotazeta new_info.info} )
	))
    g
    
let terminate_app_rec (f,args) expr_info continuation_tac _ = 
  List.iter (check_not_nested (expr_info.f_id::expr_info.forbidden_ids))
    args;
  begin
    try 
      let v = List.assoc args expr_info.args_assoc in 
      let new_infos = {expr_info with info = v} in 
      tclTHENLIST[
	continuation_tac new_infos;
	if expr_info.is_final && expr_info.is_main_branch 
	then
	  tclTHENLIST[ 
	    observe_tac (str "first split")
	      (split (ImplicitBindings [new_infos.info]));
	    observe_tac (str "destruct_bounds (3)")
	      (destruct_bounds new_infos)
	  ]
	else
	  tclIDTAC			       
      ]    
    with Not_found -> 
      observe_tac (str "terminate_app_rec not found") (tclTHENS
	(simplest_elim (mkApp(mkVar expr_info.ih,Array.of_list args)))
	[		
	  tclTHENLIST[
	    intro_using rec_res_id;
	    intro;
	    onNthHypId 1 
	      (fun v_bound -> 
		(onNthHypId 2 
		   (fun v ->  
		     let new_infos = { expr_info with 
		       info = (mkVar v); 
		       values_and_bounds = 
			 (v,v_bound)::expr_info.values_and_bounds; 
		       args_assoc=(args,mkVar v)::expr_info.args_assoc
		     } in
		     tclTHENLIST[
		       continuation_tac new_infos;
		       if expr_info.is_final && expr_info.is_main_branch 
		       then
			 tclTHENLIST[ 
			   observe_tac (str "first split") 
			     (split (ImplicitBindings [new_infos.info]));
			   observe_tac (str "destruct_bounds (2)") 
			     (destruct_bounds new_infos)
			 ]
		       else
			 tclIDTAC			       
		     ]    
		   )
		)
	      )
	  ];
	  observe_tac (str "proving decreasing") (
	    tclTHENS (* proof of args < formal args *)
	      (apply (Lazy.force expr_info.acc_inv))
	      [ 
		observe_tac (str "h_assumption") h_assumption;
		tclTHENLIST
		  [
		    tclTRY(list_rewrite true 
			     (List.map 
				(fun e -> mkVar e,true) 
				expr_info.eqs
			     )
		    );
		    tclUSER expr_info.concl_tac true 
		      (Some (
		      expr_info.ih::expr_info.acc_id::
			(fun (x,y) -> y) 
			(List.split expr_info.values_and_bounds)
		       )
		      );
		  ]
	      ])
	])
  end

let terminate_info = 
  { message = "prove_terminate with term ";
    letiN = terminate_letin;
    lambdA = (fun _ _ _ _ -> assert false);
    casE = terminate_case;
    otherS = terminate_others;
    apP = terminate_app;
    app_reC = terminate_app_rec;
  }

let prove_terminate = travel terminate_info


(* Equation proof *)

let equation_case next_step (ci,a,t,l) expr_info continuation_tac infos = 
  terminate_case next_step (ci,a,t,l) expr_info continuation_tac infos

let rec prove_le g = 
  let x,z = 
    let _,args = decompose_app (pf_concl g) in 
    (List.hd args,List.hd (List.tl args))
  in 
  tclFIRST[
    h_assumption;
    apply (delayed_force le_n);
    begin
      try
	let matching_fun = 
	  pf_is_matching g
	    (Pattern.PApp(Pattern.PRef (reference_of_constr (le ())),[|Pattern.PVar (destVar x);Pattern.PMeta None|])) in 
	let (h,t) = List.find (fun (_,t) -> matching_fun t) (pf_hyps_types g)
	in 
	let y = 
	  let _,args = decompose_app t in 
	  List.hd (List.tl args)
	in 
	tclTHENLIST[
	  apply(mkApp(le_trans (),[|x;y;z;mkVar h|]));
	  observe_tac (str "prove_le (rec)") (prove_le)
	] 
      with Not_found -> tclFAIL 0 (mt())
    end;
  ]
    g

let rec make_rewrite_list expr_info max = function 
  | [] -> tclIDTAC
  | (_,p,hp)::l -> 
    observe_tac (str "make_rewrite_list") (tclTHENS
      (observe_tac (str "rewrite heq on " ++ pr_id p ) (
	(fun g -> 
	  let t_eq = compute_renamed_type g (mkVar hp) in
	  let k,def =
	    let k_na,_,t = destProd t_eq in
	    let _,_,t  = destProd t in
	    let def_na,_,_ = destProd t in
	    Nameops.out_name k_na,Nameops.out_name def_na
	  in
	  general_rewrite_bindings false Locus.AllOccurrences
	    true (* dep proofs also: *) true 
	    (mkVar hp,
	     ExplicitBindings[Loc.ghost,NamedHyp def,
			      expr_info.f_constr;Loc.ghost,NamedHyp k,
			      (f_S max)]) false g) )
      )
      [make_rewrite_list expr_info max l;
       tclTHENLIST[ (* x < S max proof *)
	 apply (delayed_force le_lt_n_Sm);
	 observe_tac (str "prove_le(2)") prove_le
       ]
      ] )

let make_rewrite expr_info l hp max = 
  tclTHENFIRST
    (observe_tac (str "make_rewrite") (make_rewrite_list expr_info max l))
    (observe_tac (str "make_rewrite") (tclTHENS
       (fun g -> 
	  let t_eq = compute_renamed_type g (mkVar hp) in
	  let k,def =
	    let k_na,_,t = destProd t_eq in
	    let _,_,t  = destProd t in
	    let def_na,_,_ = destProd t in
	    Nameops.out_name k_na,Nameops.out_name def_na
	  in
	 observe_tac (str "general_rewrite_bindings")
	   (general_rewrite_bindings false Locus.AllOccurrences
	    true (* dep proofs also: *) true 
	    (mkVar hp,
	     ExplicitBindings[Loc.ghost,NamedHyp def,
			      expr_info.f_constr;Loc.ghost,NamedHyp k,
			      (f_S (f_S max))]) false) g)
       [observe_tac(str "make_rewrite finalize") (
	 (* tclORELSE( h_reflexivity) *)
	 (tclTHENLIST[
	   simpl_iter Locusops.onConcl;
	   observe_tac (str "unfold functional")
	     (unfold_in_concl[(Locus.OnlyOccurrences [1],
			       evaluable_of_global_reference expr_info.func)]);
	   
	   (list_rewrite true 
	      (List.map (fun e -> mkVar e,true) expr_info.eqs));
	   (observe_tac (str "h_reflexivity") h_reflexivity)]))
       ;
	 tclTHENLIST[ (* x < S (S max) proof *)
	 apply (delayed_force le_lt_SS);
	 observe_tac (str "prove_le (3)") prove_le
	 ]
       ])  
    )

let rec compute_max rew_tac max l = 
  match l with 
    | [] -> rew_tac max
    | (_,p,_)::l -> 
      tclTHENLIST[
	simplest_elim 
	  (mkApp(delayed_force max_constr, [| max; mkVar p|]));
	tclDO 3 intro;
	onNLastHypsId 3 (fun lids -> 
	  match lids with
	    | [hle2;hle1;pmax] -> compute_max rew_tac (mkVar pmax) l 
	    | _ -> assert false
	)]

let rec destruct_hex expr_info acc l = 
  match l with 
    | [] -> 
      begin
	match List.rev acc with 
	  | [] -> tclIDTAC 
	  | (_,p,hp)::tl  -> 
	    observe_tac (str "compute max ") (compute_max (make_rewrite expr_info tl hp) (mkVar p) tl)
      end
    | (v,hex)::l -> 
      tclTHENLIST[
	simplest_case (mkVar hex);
	clear [hex];
	tclDO 2 intro;
	onNthHypId 1 (fun hp -> 
	  onNthHypId 2 (fun p -> 
	    observe_tac 
	      (str "destruct_hex after " ++ pr_id hp ++ spc () ++ pr_id p)
	      (destruct_hex expr_info ((v,p,hp)::acc) l)
	  )
	)
      ]
	
let rec intros_values_eq expr_info acc = 
  tclORELSE(
    tclTHENLIST[
      tclDO 2 intro;
      onNthHypId 1 (fun hex -> 
	(onNthHypId 2 (fun v -> intros_values_eq expr_info ((v,hex)::acc)))
      )
    ])
    (tclCOMPLETE (
      destruct_hex expr_info [] acc
    ))

let equation_others _ expr_info continuation_tac infos = 
  if expr_info.is_final && expr_info.is_main_branch 
  then 
    tclTHEN 
      (continuation_tac infos) 
      (intros_values_eq expr_info [])
  else continuation_tac infos

let equation_letin (na,b,t,e) expr_info continuation_tac info = 
  let new_e = subst1 info.info e in 
  continuation_tac {info with info = new_e;} 

let equation_app f_and_args expr_info continuation_tac infos = 
    if expr_info.is_final && expr_info.is_main_branch 
    then intros_values_eq expr_info []
    else continuation_tac infos
	    
let equation_app_rec (f,args) expr_info continuation_tac info = 
  begin
    try
      let v = List.assoc args expr_info.args_assoc in
      let new_infos = {expr_info with info = v} in
      observe_tac (str "app_rec found") (continuation_tac new_infos)
    with Not_found ->
      if expr_info.is_final && expr_info.is_main_branch 
      then 
	tclTHENLIST
	  [ simplest_case (mkApp (expr_info.f_terminate,Array.of_list args));
	    continuation_tac {expr_info with args_assoc = (args,delayed_force coq_O)::expr_info.args_assoc};
	    observe_tac (str "app_rec intros_values_eq") (intros_values_eq expr_info [])
	  ]
      else 
	tclTHENLIST[
  	  simplest_case (mkApp (expr_info.f_terminate,Array.of_list args));
	  observe_tac (str "app_rec not_found") (continuation_tac {expr_info with args_assoc = (args,delayed_force coq_O)::expr_info.args_assoc})
	]
  end

let equation_info = 
  {message = "prove_equation with term ";
   letiN = (fun _ -> assert false);
   lambdA = (fun _ _ _ _ -> assert false);
   casE = equation_case;
   otherS = equation_others;
   apP = equation_app;
   app_reC = equation_app_rec
}
    
let prove_eq = travel equation_info

(* wrappers *)
(* [compute_terminate_type] computes the type of the Definition f_terminate from the type of f_F
*)
let compute_terminate_type nb_args func =
  let _,a_arrow_b,_ = destLambda(def_of_const (constr_of_global func)) in
  let rev_args,b = decompose_prod_n nb_args a_arrow_b in
  let left =
    mkApp(delayed_force iter,
	  Array.of_list
	    (lift 5 a_arrow_b:: mkRel 3::
	       constr_of_global func::mkRel 1::
	       List.rev (list_map_i (fun i _ -> mkRel (6+i)) 0 rev_args)
	    )
	 )
  in
  let right = mkRel 5 in
  let equality = mkApp(delayed_force eq, [|lift 5 b; left; right|]) in
  let result = (mkProd ((Name def_id) , lift 4 a_arrow_b, equality)) in
  let cond = mkApp(delayed_force lt, [|(mkRel 2); (mkRel 1)|]) in
  let nb_iter =
    mkApp(delayed_force ex,
	  [|delayed_force nat;
	    (mkLambda
	       (Name
		  p_id,
		  delayed_force nat,
		  (mkProd (Name k_id, delayed_force nat,
			   mkArrow cond result))))|])in
  let value = mkApp(delayed_force coq_sig,
		    [|b;
		      (mkLambda (Name v_id, b, nb_iter))|]) in
  compose_prod rev_args value


let termination_proof_header is_mes input_type ids args_id relation
    rec_arg_num rec_arg_id tac wf_tac : tactic =
  begin
    fun g ->
      let nargs = List.length args_id in
      let pre_rec_args =
	List.rev_map
	  mkVar (fst (list_chop (rec_arg_num - 1) args_id))
      in
      let relation = substl pre_rec_args relation in
      let input_type = substl pre_rec_args input_type in
      let wf_thm = next_ident_away_in_goal (id_of_string ("wf_R")) ids in
      let wf_rec_arg =
	next_ident_away_in_goal
	  (id_of_string ("Acc_"^(string_of_id rec_arg_id)))
	  (wf_thm::ids)
      in
      let hrec = next_ident_away_in_goal hrec_id
	(wf_rec_arg::wf_thm::ids) in
      let acc_inv =
	  lazy (
	    mkApp (
	      delayed_force acc_inv_id,
	      [|input_type;relation;mkVar rec_arg_id|]
	    )
	  )
      in
      tclTHEN
	(h_intros args_id)
	(tclTHENS
	   (observe_tac
	      (str "first assert")
	      (assert_tac
		 (Name wf_rec_arg)
		 (mkApp (delayed_force acc_rel,
			 [|input_type;relation;mkVar rec_arg_id|])
		 )
	      )
	   )
	   [
	     (* accesibility proof *)
	     tclTHENS
	       (observe_tac
		  (str "second assert")
		  (assert_tac
		     (Name wf_thm)
		     (mkApp (delayed_force well_founded,[|input_type;relation|]))
		  )
	       )
	       [
		 (* interactive proof that the relation is well_founded *)
		 observe_tac (str "wf_tac") (wf_tac is_mes (Some args_id));
		 (* this gives the accessibility argument *)
		 observe_tac
		   (str "apply wf_thm")
		   (h_simplest_apply (mkApp(mkVar wf_thm,[|mkVar rec_arg_id|]))
		   )
	       ]
	     ;
	     (* rest of the proof *)
	     tclTHENSEQ
	       [observe_tac (str "generalize")
		  (onNLastHypsId (nargs+1)
		     (tclMAP (fun id ->
			tclTHEN (h_generalize [mkVar id]) (h_clear false [id]))
		     ))
	       ;
		observe_tac (str "h_fix") (h_fix (Some hrec) (nargs+1));
		h_intros args_id;
		h_intro wf_rec_arg;
		observe_tac (str "tac") (tac wf_rec_arg hrec wf_rec_arg acc_inv)
	       ]
	   ]
	) g
  end



let rec instantiate_lambda t l =
  match l with
  | [] -> t
  | a::l ->
      let (_, _, body) = destLambda t in
      instantiate_lambda (subst1 a body) l

let whole_start (concl_tac:tactic) nb_args is_mes func input_type relation rec_arg_num  : tactic =
  begin
    fun g ->
      let ids = Termops.ids_of_named_context (pf_hyps g) in
      let func_body = (def_of_const (constr_of_global func)) in
      let (f_name, _, body1) = destLambda func_body in
      let f_id =
	match f_name with
	  | Name f_id -> next_ident_away_in_goal f_id ids
	  | Anonymous -> anomaly "Anonymous function"
      in
      let n_names_types,_ = decompose_lam_n nb_args body1 in
      let n_ids,ids =
	List.fold_left
	  (fun (n_ids,ids) (n_name,_) ->
	     match n_name with
	       | Name id ->
		   let n_id = next_ident_away_in_goal id ids in
		   n_id::n_ids,n_id::ids
	       | _ -> anomaly "anonymous argument"
	  )
	  ([],(f_id::ids))
	  n_names_types
      in
      let rec_arg_id = List.nth n_ids (rec_arg_num - 1) in
      let expr = instantiate_lambda func_body (mkVar f_id::(List.map mkVar n_ids)) in
      termination_proof_header
	is_mes
	input_type
	ids
	n_ids
	relation
	rec_arg_num
	rec_arg_id
	(fun rec_arg_id hrec acc_id acc_inv g -> 	      
	  (prove_terminate (fun infos -> tclIDTAC) 
	     { is_main_branch = true; (* we are on the main branche (i.e. still on a match ... with .... end *)
	       is_final = true;      (* and on leaf (more or less) *)
	       f_terminate = delayed_force coq_O;
	       nb_arg = nb_args;
	       concl_tac = concl_tac;
	       rec_arg_id = rec_arg_id;
	       is_mes = is_mes;
	       ih = hrec;
	       f_id = f_id;
	       f_constr = mkVar f_id;
	       func = func;
	       info = expr;
	       acc_inv = acc_inv;
	       acc_id = acc_id;
	       values_and_bounds = [];
	       eqs = [];
	       forbidden_ids = [];
	       args_assoc = []
	     }
	  ) 
	     g
	)
	(tclUSER_if_not_mes concl_tac)
	g
  end

let get_current_subgoals_types () =
  let p = Proof_global.give_me_the_proof () in
  let { Evd.it=sgs ; sigma=sigma } = Proof.V82.subgoals p in
  List.map (Goal.V82.abstract_type sigma) sgs

let build_and_l l =
  let and_constr =  Coqlib.build_coq_and () in
  let conj_constr = coq_conj () in
  let mk_and p1 p2 =
    Term.mkApp(and_constr,[|p1;p2|]) in
  let rec is_well_founded t = 
    match kind_of_term t with 
      | Prod(_,_,t') -> is_well_founded t'
      | App(_,_) -> 
	let (f,_) = decompose_app t in 
	eq_constr f (well_founded ())
      | _ -> assert false
  in
  let compare t1 t2 = 
    let b1,b2= is_well_founded t1,is_well_founded t2 in 
    if (b1&&b2) || not (b1 || b2) then 0
    else if b1 && not b2 then 1 else -1
  in
  let l = List.sort compare l in 
  let rec f  = function
    | [] -> failwith "empty list of subgoals!"
    | [p] -> p,tclIDTAC,1
    | p1::pl ->
	let c,tac,nb = f pl in
	mk_and p1 c,
	tclTHENS
	  (apply (constr_of_global conj_constr))
	  [tclIDTAC;
	   tac
	  ],nb+1
  in f l


let is_rec_res id =
  let rec_res_name = string_of_id rec_res_id   in
  let id_name = string_of_id id in
  try
    String.sub id_name 0 (String.length rec_res_name) = rec_res_name
  with _ -> false
 
let clear_goals =
  let rec clear_goal t =
    match kind_of_term t with
      | Prod(Name id as na,t',b) ->
	  let b' = clear_goal b in
	  if noccurn 1 b' && (is_rec_res id)
	  then Termops.pop b'
	  else if b' == b then t
	  else mkProd(na,t',b')
      | _ -> map_constr clear_goal t
  in
  List.map clear_goal


let build_new_goal_type () =
  let sub_gls_types = get_current_subgoals_types () in
  (* Pp.msgnl (str "sub_gls_types1 := " ++ Util.prlist_with_sep (fun () -> Pp.fnl () ++ Pp.fnl ()) Printer.pr_lconstr sub_gls_types); *)
  let sub_gls_types = clear_goals sub_gls_types in
  (* Pp.msgnl (str "sub_gls_types2 := " ++ Util.prlist_with_sep (fun () -> Pp.fnl () ++ Pp.fnl ()) Printer.pr_lconstr sub_gls_types); *)
  let res = build_and_l sub_gls_types in
  res

let is_opaque_constant c =
  let cb = Global.lookup_constant c in
  match cb.Declarations.const_body with
    | Declarations.OpaqueDef _ -> true
    | Declarations.Undef _ -> true
    | Declarations.Def _ -> false

let open_new_goal (build_proof:tactic -> tactic -> unit) using_lemmas ref_ goal_name (gls_type,decompose_and_tac,nb_goal)   =
  (* Pp.msgnl (str "gls_type := " ++ Printer.pr_lconstr gls_type); *)
  let current_proof_name = get_current_proof_name () in
  let name = match goal_name with
    | Some s -> s
    | None   ->
	try (add_suffix current_proof_name "_subproof")
	with _ -> anomaly "open_new_goal with an unamed theorem"
  in
  let sign = initialize_named_context_for_proof () in
  let na = next_global_ident_away name [] in
  if Termops.occur_existential gls_type then
    Errors.error "\"abstract\" cannot handle existentials";
  let hook _ _ =
    let opacity =
      let na_ref = Libnames.Ident (Loc.ghost,na) in
      let na_global = Nametab.global na_ref in
      match na_global with
	  ConstRef c -> is_opaque_constant c
	| _ -> anomaly "equation_lemma: not a constant"
    in
    let lemma = mkConst (Lib.make_con na) in
    ref_ := Some lemma ;
    let lid = ref [] in
    let h_num = ref (-1) in
    ignore (Flags.silently Vernacentries.interp (Vernacexpr.VernacAbort None));
    build_proof
      (  fun gls ->
	   let hid = next_ident_away_in_goal h_id (pf_ids_of_hyps gls) in
	   tclTHENSEQ
	     [
	       h_generalize [lemma];
	       h_intro hid;
	       (fun g ->
		  let ids = pf_ids_of_hyps g in
		  tclTHEN
		    (Elim.h_decompose_and (mkVar hid))
		    (fun g ->
		       let ids' = pf_ids_of_hyps g in
		       lid := List.rev (list_subtract ids' ids);
		       if !lid = [] then lid := [hid];
		       tclIDTAC g
		    )
		    g
	       );
	     ] gls)
      (fun g ->
	 match kind_of_term (pf_concl g) with
	   | App(f,_) when eq_constr f (well_founded ()) ->
	       Auto.h_auto None [] (Some [])  g
	   | _ ->
	       incr h_num;
	       (observe_tac (str "finishing using")
		  (
		    tclCOMPLETE(
		      tclFIRST[
			tclTHEN
			  (eapply_with_bindings (mkVar (List.nth !lid !h_num), NoBindings))
			  e_assumption;
		      Eauto.eauto_with_bases
			(true,5)
			[Evd.empty,Lazy.force refl_equal]
			[Auto.Hint_db.empty empty_transparent_state false]
		      ]
		    )
		  )
	       )
      		 g)
;
    Lemmas.save_named opacity;
  in
  start_proof
    na
    (Decl_kinds.Global, Decl_kinds.Proof Decl_kinds.Lemma)
    sign
    gls_type
    hook ;
  if Indfun_common.is_strict_tcc  ()
  then
    by (tclIDTAC)
  else 
    begin
      by (
	fun g ->
	  tclTHEN
	    (decompose_and_tac)
	    (tclORELSE
	       (tclFIRST
	 	  (List.map
	 	     (fun c ->
	 		tclTHENSEQ
	 		  [intros;
	 		   h_simplest_apply (interp_constr Evd.empty (Global.env()) c);
	 		   tclCOMPLETE Auto.default_auto
	 		  ]
	 	     )
	 	     using_lemmas)
	       ) tclIDTAC)
	    g)
    end;
  try
    by tclIDTAC; (* raises UserError _ if the proof is complete *)
  with UserError _ ->
    defined ()



let com_terminate
    tcc_lemma_name
    tcc_lemma_ref
    is_mes
    fonctional_ref
    input_type
    relation
    rec_arg_num
    thm_name using_lemmas
    nb_args
    hook =
  let start_proof (tac_start:tactic) (tac_end:tactic) =
    let (evmap, env) = Lemmas.get_current_context() in
    start_proof thm_name
      (Global, Proof Lemma) (Environ.named_context_val env)
      (compute_terminate_type nb_args fonctional_ref) hook;

    by (observe_tac (str "starting_tac") tac_start);
    by (observe_tac (str "whole_start") (whole_start tac_end nb_args is_mes fonctional_ref
    				   input_type relation rec_arg_num ))
  in
  start_proof tclIDTAC tclIDTAC;
  try
    let new_goal_type = build_new_goal_type () in
    open_new_goal start_proof using_lemmas tcc_lemma_ref
      (Some tcc_lemma_name)
      (new_goal_type);
  with Failure "empty list of subgoals!" ->
    (* a non recursive function declared with measure ! *)
    defined ()





let start_equation (f:global_reference) (term_f:global_reference)
  (cont_tactic:identifier list -> tactic) g =
  let ids = pf_ids_of_hyps g in
  let terminate_constr = constr_of_global term_f in
  let nargs = nb_prod (type_of_const terminate_constr) in
  let x = n_x_id ids nargs in
  tclTHENLIST [
    h_intros x;
    unfold_in_concl [(Locus.AllOccurrences, evaluable_of_global_reference f)];
    observe_tac (str "simplest_case")
      (simplest_case (mkApp (terminate_constr,
                             Array.of_list (List.map mkVar x))));
    observe_tac (str "prove_eq") (cont_tactic x)] g;;

let (com_eqn : int -> identifier ->
       global_reference -> global_reference -> global_reference
	 -> constr -> unit) =
  fun nb_arg eq_name functional_ref f_ref terminate_ref equation_lemma_type ->
    let opacity =
      match terminate_ref with
	| ConstRef c -> is_opaque_constant c
	| _ -> anomaly "terminate_lemma: not a constant"
    in
    let (evmap, env) = Lemmas.get_current_context() in
    let f_constr = constr_of_global f_ref in
    let equation_lemma_type = subst1 f_constr equation_lemma_type in
    (start_proof eq_name (Global, Proof Lemma)
       (Environ.named_context_val env) equation_lemma_type (fun _ _ -> ());
     by
       (start_equation f_ref terminate_ref
	  (fun  x ->
	     prove_eq (fun _ -> tclIDTAC)
	       {nb_arg=nb_arg;
		f_terminate = constr_of_global terminate_ref; 
	        f_constr = f_constr; 
		concl_tac = tclIDTAC;
		func=functional_ref;
		info=(instantiate_lambda
	       		(def_of_const (constr_of_global functional_ref))
	       		(f_constr::List.map mkVar x)
		);
		is_main_branch = true;
		is_final = true;
		values_and_bounds = [];
		eqs = [];
		forbidden_ids = [];
		acc_inv = lazy (assert false);
		acc_id = id_of_string "____";
		args_assoc = [];
		f_id = id_of_string "______";
		rec_arg_id = id_of_string "______";
		is_mes = false;
		ih = id_of_string "______";
	       }
	  )
       ); 
     (* (try Vernacentries.interp (Vernacexpr.VernacShow Vernacexpr.ShowProof) with _ -> ()); *)
(*      Vernacentries.interp (Vernacexpr.VernacShow Vernacexpr.ShowScript); *)
     Flags.silently (fun () -> Lemmas.save_named opacity) () ; 
(*      Pp.msgnl (str "eqn finished"); *)
    );;


let recursive_definition is_mes function_name rec_impls type_of_f r rec_arg_num eq
    generate_induction_principle using_lemmas : unit =
  let previous_label = Lib.current_command_label () in
  let function_type = interp_constr Evd.empty (Global.env()) type_of_f in
  let env = push_named (function_name,None,function_type) (Global.env()) in
  (* Pp.msgnl (str "function type := " ++ Printer.pr_lconstr function_type);  *)
  let equation_lemma_type = 
    nf_betaiotazeta
      (interp_gen (OfType None) Evd.empty env ~impls:rec_impls eq) 
  in
 (* Pp.msgnl (str "lemma type := " ++ Printer.pr_lconstr equation_lemma_type ++ fnl ()); *)
  let res_vars,eq' = decompose_prod equation_lemma_type in
  let env_eq' = Environ.push_rel_context (List.map (fun (x,y) -> (x,None,y)) res_vars) env in
  let eq' = nf_zeta env_eq' eq'  in
  let res =
(*     Pp.msgnl (str "res_var :=" ++ Printer.pr_lconstr_env (push_rel_context (List.map (function (x,t) -> (x,None,t)) res_vars) env) eq'); *)
(*     Pp.msgnl (str "rec_arg_num := " ++ str (string_of_int rec_arg_num)); *)
(*     Pp.msgnl (str "eq' := " ++ str (string_of_int rec_arg_num)); *)
    match kind_of_term eq' with
      | App(e,[|_;_;eq_fix|]) ->
	  mkLambda (Name function_name,function_type,subst_var function_name (compose_lam res_vars  eq_fix))
      | _ -> failwith "Recursive Definition (res not eq)"
  in
  let pre_rec_args,function_type_before_rec_arg = decompose_prod_n (rec_arg_num - 1) function_type in
  let (_, rec_arg_type, _) = destProd function_type_before_rec_arg in
  let arg_types = List.rev_map snd (fst (decompose_prod_n (List.length res_vars) function_type)) in
  let equation_id = add_suffix function_name "_equation" in
  let functional_id =  add_suffix function_name "_F" in
  let term_id = add_suffix function_name "_terminate" in
  let functional_ref = declare_fun functional_id (IsDefinition Decl_kinds.Definition) res in
  let env_with_pre_rec_args = push_rel_context(List.map (function (x,t) -> (x,None,t)) pre_rec_args) env in  
  let relation =
    interp_constr
      Evd.empty
      env_with_pre_rec_args
      r
  in
  let tcc_lemma_name = add_suffix function_name "_tcc" in
  let tcc_lemma_constr = ref None in
  (* let _ = Pp.msgnl (str "relation := " ++ Printer.pr_lconstr_env env_with_pre_rec_args relation) in *)
  let hook _ _ = 
    let term_ref = Nametab.locate (qualid_of_ident term_id) in
    let f_ref = declare_f function_name (IsProof Lemma) arg_types term_ref in
    let _ = Table.extraction_inline true [Ident (Loc.ghost,term_id)] in 
    (*     message "start second proof"; *)
    let stop = 
      try com_eqn (List.length res_vars) equation_id functional_ref f_ref term_ref (subst_var function_name equation_lemma_type);
	  false
      with e ->
	begin
	  if do_observe ()
	  then msg_debug (str "Cannot create equation Lemma " ++ Errors.print e)
	  else anomaly "Cannot create equation Lemma"
	  ;
	  true
	end
    in
    if not stop
    then
      let eq_ref = Nametab.locate (qualid_of_ident equation_id ) in
      let f_ref = destConst (constr_of_global f_ref)
      and functional_ref = destConst (constr_of_global functional_ref)
      and eq_ref = destConst (constr_of_global eq_ref) in
      generate_induction_principle f_ref tcc_lemma_constr
	functional_ref eq_ref rec_arg_num rec_arg_type (nb_prod res) relation;
      if Flags.is_verbose ()
      then msgnl (h 1 (Ppconstr.pr_id function_name ++
			 spc () ++ str"is defined" )++ fnl () ++
		    h 1 (Ppconstr.pr_id equation_id ++
			   spc () ++ str"is defined" )
      )
  in
  try 
    com_terminate
      tcc_lemma_name
      tcc_lemma_constr
      is_mes functional_ref
      rec_arg_type
      relation rec_arg_num
      term_id
      using_lemmas
      (List.length res_vars)
      hook 
  with e ->
    begin
      (try ignore (Backtrack.backto previous_label) with _ -> ());
      (*       anomaly "Cannot create termination Lemma" *)
      raise e
    end

