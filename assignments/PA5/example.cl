
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class A inherits Main {};
class B inherits Main {};
class C inherits B {};
class D {};

class Main inherits IO
{
  a : Int;
  main() : Object
  {
    let thing : Object <- self in
      case thing of
        o : Object => out_string( "object\n" );
	      m : Main => out_string( "main\n" );
        m : A => out_string( "A\n" );
        m : B => out_string( "B\n" );
        m : C => out_string( "C\n" );
        m : D => out_string( "D\n" );
      esac
  };
};
