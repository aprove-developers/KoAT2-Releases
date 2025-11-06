open Base

let print_logo () =
  let koat_string =
    [
      "██╗  ██╗   ██████╗    █████╗   ████████╗";
      "██║ ██╔╝  ██╔═══██╗  ██╔══██╗  ╚══██╔══╝";
      "█████╔╝   ██║   ██║  ███████║     ██║";
      "██╔═██╗   ██║   ██║  ██╔══██║     ██║";
      "██║  ██╗  ╚██████╔╝  ██║  ██║     ██║";
      "╚═╝  ╚═╝   ╚═════╝   ╚═╝  ╚═╝     ╚═╝";
    ]
  in

  let gradient_colors =
    [
      "rgb(255,128,0)";
      (* bright orange *)
      "rgb(255,90,54)";
      (* orange-red *)
      "rgb(172,65,66)";
      (* #ac4142 - warm red *)
      "rgb(139,44,128)";
      (* purple-magenta *)
      "rgb(89,0,89)";
      (* dark purple *)
      "rgb(61,0,61)";
      (* very dark purple *)
    ]
  in

  print_newline ();
  List.iteri
    ~f:(fun i line ->
      let color = List.nth_exn gradient_colors i in
      Ocolor_format.printf "@{<%s>%s@}@\n" color line)
    koat_string;
  print_newline ();
  print_newline ()
