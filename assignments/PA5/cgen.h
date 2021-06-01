#include <assert.h>
#include <stdio.h>
#include <unordered_map>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <typeinfo>
#include <map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

struct AttrInfo {
   int offset;
   Symbol type;
};

struct VtblInfo {
   int offset;
   std::string label;
};

class ObjectLayout {
   int next_attr_off;
   int next_vtbl_off;

   public:
   Symbol filename;
   std::unordered_map<Symbol, AttrInfo> attrs;
   std::unordered_map<Symbol, VtblInfo> vtbl;

   int tag;
   int max_child_tag;

   public:
   ObjectLayout() : next_attr_off(DEFAULT_OBJFIELDS), next_vtbl_off(0) {}
   void add_attr(Symbol name, Symbol type);
   void add_method(Symbol name, Symbol cls);
};


class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

   std::unordered_map<Symbol, ObjectLayout> layouts;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   void build_class_layout();
   void code_class_table();
   void code_dispatch_table();
   void code_proto_obj();
   void code_init_method();
   void code_class_methods();

public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   ObjectLayout layout;
   int tag;
   SymbolTable<Symbol, Position> env;

   static int next_tag;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   void set_tag(int t) { tag = t; }
   void set_tag() { tag = next_tag++; }
   int get_tag() { return tag; }
   Symbol get_name() { return name; }

   int build_class_layout(ObjectLayout layout, std::unordered_map<Symbol, ObjectLayout>& layouts);
   void code_dispatch_table(ostream& s);
   void code_proto_obj(ostream& s);
   void code_init_method(ostream& s, std::unordered_map<Symbol, ObjectLayout>&);
   void code_class_methods(ostream& s, std::unordered_map<Symbol, ObjectLayout>&);
   void gen_init_env();
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

enum Location {
   Obj,
   Stack
};

struct Position {
   Location loc;
   int offset;
};
