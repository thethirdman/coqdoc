open Ocamlbuild_plugin

(*include Myocamlbuild_config*)

let coqsrc="/home/ripault/Prog/coq/trunk"

let _ = dispatch begin function
  | Before_options -> Options.use_ocamlfind := true
  | After_rules ->
    ocaml_lib ~extern:true ~dir:(coqsrc/"lib") "clib";
    flag ["compile";"use_clib"] (S[A"-I" ; P(coqsrc/"lib")]);
    ocaml_lib ~extern:true ~dir:(coqsrc/"lib") "lib";
    flag ["compile";"use_lib"] (S[A"-I" ; P(coqsrc/"lib")]);
  | _ -> ()
end
