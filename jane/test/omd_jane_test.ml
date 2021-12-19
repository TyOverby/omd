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
  run_test {| 
  ---
  title: hi
  ---

  this is a test |};
  [%expect
    {|
    === sexp ===
    ((Thematic_break ()) (Heading ((attrs ()) (level 2) (content ("title: hi"))))
     (Paragraph () ("this is a test")))

    === html ===
    <hr>
    <h2>
     title: hi
    </h2>
    <p>
     this is a test
    </p> |}]

let%expect_test _ =
  run_test {| this is a test |};
  [%expect
    {|
    === sexp ===
    ((Paragraph () ("this is a test")))

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
    ((Heading ((attrs ()) (level 1) (content (h1))))
     (Heading ((attrs ()) (level 2) (content (h2))))
     (Heading ((attrs ()) (level 1) (content (h1)))))

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
    ((Html_block (attrs ())
      (content_raw  "  <section>\
                   \n  # h1 \
                   \n  ## h2\
                   \n  # h1\
                   \n  </section>\
                   \n")))

    === html ===
    <section>
     # h1
      ## h2
      # h1
    </section> |}]

let%expect_test _ =
  run_test {| 
  - this
    is
    a 
    test
  - foo
  |};
  [%expect
    {|
    === sexp ===
    ((List (attrs ()) (kind (Bullet -)) (spacing Tight)
      (blocks
       (((Paragraph ()
          (this (Soft_break ()) is (Soft_break ()) a (Soft_break ()) test)))
        ((Paragraph () (foo)))))))

    === html ===
    <ul>
     <li>
      this
    is
    a
    test
     </li>
     <li>
      foo
     </li>
    </ul> |}]

let%expect_test _ =
  run_test
    {| 
  - this
    ```ocaml
    let x = 10
    ```
  - ```ocaml
    let y = 20
    ```
  |};
  [%expect
    {|
    === sexp ===
    ((List (attrs ()) (kind (Bullet -)) (spacing Tight)
      (blocks
       (((Paragraph () (this))
         (Code_block (attrs ()) (language ocaml) (content "let x = 10\n")))
        ((Code_block (attrs ()) (language ocaml) (content "let y = 20\n")))))))

    === html ===
    <ul>
     <li>
      this
    <pre><code class="language-ocaml">let x = 10
    </code></pre>
     </li>
     <li>
      <pre><code class="language-ocaml">let y = 20
    </code></pre>
     </li>
    </ul> |}]

let%expect_test _ =
  run_test
    {| 
  - this
  - is
    1. a 
    2. bulleted 
    3. list
    4. with
      - [ ] some
      - [x] bullets
  |};
  [%expect
    {|
    === sexp ===
    ((List (attrs ()) (kind (Bullet -)) (spacing Tight)
      (blocks
       (((Paragraph () (this)))
        ((Paragraph () (is))
         (List (attrs ()) (kind (Ordered 1 .)) (spacing Tight)
          (blocks
           (((Paragraph () (a))) ((Paragraph () (bulleted)))
            ((Paragraph () (list))) ((Paragraph () (with))))))
         (List (attrs ()) (kind (Bullet -)) (spacing Tight)
          (blocks
           (((Paragraph () ("[ ] some"))) ((Paragraph () ("[x] bullets")))))))))))

    === html ===
    <ul>
     <li>
      this
     </li>
     <li>
      is
      <ol>
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
       </li>
      </ol>
      <ul>
       <li>
        [ ] some
       </li>
       <li>
        [x] bullets
       </li>
      </ul>
     </li>
    </ul> |}]
