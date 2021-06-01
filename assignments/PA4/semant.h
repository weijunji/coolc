#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <unordered_map>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

struct ClassInfo;

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  std::unordered_map<Symbol, ClassInfo*> tbl;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  void check_inheritance();
  ClassInfo* get_cls_info(Symbol, Symbol);
  void build_mtbl();
  void check_main();
  bool is_subclass(Symbol cls, Symbol sub, Symbol parent);
  Symbol find_lca(Symbol cls, Symbol a, Symbol b); // lower common ancestor
};


struct MethodInfo {
  Symbol cls;
  method_class* method;
};

struct ClassInfo {
  Class_ cls;
  std::unordered_map<Symbol, MethodInfo> mtbl;
  std::vector<ClassInfo*> children;

  ClassInfo(Class_ cls) : cls(cls){}
  void build_mtbl(ClassTableP ctbl);
};

#endif
