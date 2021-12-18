open! Base

let run_test s =
  let t = Omd_jane.Document.of_string s in
  Stdlib.print_endline "=== sexp ===";
  t |> Omd_jane.Document.sexp_of_t |> Sexp.to_string_hum |> Stdlib.print_endline;
  Stdlib.print_endline "\n=== html ===";
  t
  |> Omd_jane.Html.of_document
  |> Omd_jane.Html.to_string
  |> Soup.parse
  |> Soup.pretty_print
  |> Stdlib.print_endline

let%expect_test _ =
  run_test {| this is a test |};
  [%expect
    {|
    === sexp ===
    ((paragraph "this is a test"))

    === html ===
    <p>
     this is a test
    </p> |}]

let%expect_test _ =
  run_test {| 
  # h1 
  ## h2
  # h1
  |};
  [%expect
    {|
    === sexp ===
    ((heading 1 h1) (heading 2 h2) (heading 1 h1))

    === html ===
    <h1>
     h1
    </h1>
    <h2>
     h2
    </h2>
    <h1>
     h1
    </h1> |}]

let%expect_test _ =
  run_test {| 
  <section>
  # h1 
  ## h2
  # h1
  </section>
  |};
  [%expect
    {|
    === sexp ===
    ((html  "  <section>\
           \n  # h1 \
           \n  ## h2\
           \n  # h1\
           \n  </section>\
           \n"))

    === html ===
    <section>
     # h1
      ## h2
      # h1
    </section> |}]

let%expect_test _ =
  run_test
    {| 
  - this
  - is
    - a 
    - bulleted 
    - list
    - with
      - [ ] some
      - [x] bullets
  |};
  [%expect
    {|
    === sexp ===
    ((list (list-item (paragraph this))
      (list-item (paragraph is)
       (list (list-item (paragraph a)) (list-item (paragraph bulleted))
        (list-item (paragraph list))
        (list-item (paragraph with)
         (list (list-item (paragraph (concat [ " " "] some")))
          (list-item (paragraph (concat [ x "] bullets")))))))))

    === html ===
    <ul>
     <li>
      this
     </li>
     <li>
      is
      <ul>
       <li>
        a
       </li>
       <li>
        bulleted
       </li>
       <li>
        list
       </li>
       <li>
        with
        <ul>
         <li>
          [ ] some
         </li>
         <li>
          [x] bullets
         </li>
        </ul>
       </li>
      </ul>
     </li>
    </ul> |}]
