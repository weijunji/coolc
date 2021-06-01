

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <symtab.h>
#include <stack>
#include <set>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    install_basic_classes();
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ cls = classes->nth(i);
        if (cls->get_name() == SELF_TYPE) {
            semant_error(cls) << "Redefinition of basic class SELF_TYPE" << endl;
        } else {
            if (!tbl.insert({cls->get_name(), new ClassInfo(cls)}).second) {
                Class_ prev = tbl[cls->get_name()]->cls;
                semant_error(cls) << "redefinition of class " << cls->get_name()
                << ", previous definition in " << prev->get_filename() << ":" << prev->get_line_number() << endl;
            }
        }
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    tbl.insert({Object, new ClassInfo(Object_class)});
    tbl.insert({IO, new ClassInfo(IO_class)});
    tbl.insert({Int, new ClassInfo(Int_class)});
    tbl.insert({Bool, new ClassInfo(Bool_class)});
    tbl.insert({Str, new ClassInfo(Str_class)});
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
}

void ClassTable::check_inheritance() {
    for(auto iter = tbl.begin(); iter != tbl.end(); ++iter){
        Class_ cls = iter->second->cls;
        Symbol parent = cls->get_parent();
        if (parent == Int || parent == Str || parent == Bool) {
            semant_error(cls) << "Cannot inherit from '" << parent << "'" << endl;
        } else {
            if (parent != No_class) {
                auto pit = tbl.find(parent);
                if (pit != tbl.end()) {
                    pit->second->children.push_back(iter->second);
                } else {
                    semant_error(cls) << "Unknown parent class '" << parent << "'" << endl;
                }
            }
        }
    }
}

void ClassInfo::build_mtbl(ClassTableP ctbl) {
    if (mtbl.size() != 0) return;
    // get parent class's mtbl
    Symbol parent = cls->get_parent();
    if (parent != No_class) {
        ClassInfo* parent_info = ctbl->get_cls_info(cls->get_name(), parent);
        if (parent_info->mtbl.size() == 0) {
            parent_info->build_mtbl(ctbl);
        }
        mtbl = parent_info->mtbl;
    }

    // insert all method info into mtbl
    Features fs = cls->get_features();
    for (int i = fs->first(); fs->more(i); i = fs->next(i)) {
        Feature feature = fs->nth(i);
        method_class* method = dynamic_cast<method_class*>(feature);
        if (method) {
            // check method signature's type
            Symbol ret = method->get_ret();
            if (ctbl->get_cls_info(cls->get_name(), ret) == nullptr) {
                ctbl->semant_error(cls->get_filename(), method) << "Undefined type '" << ret << "'" << endl;
            }
            std::vector<Symbol> args = method->get_args();
            for (auto arg : args) {
                if (ctbl->get_cls_info(cls->get_name(), arg) == nullptr) {
                    ctbl->semant_error(cls->get_filename(), method) << "Undefined type '" << arg << "'" << endl;
                }
            }

            // insert into mtbl
            MethodInfo info{cls->get_name(), method};
            Symbol name = method->get_name();

            if (!mtbl.insert({name, info}).second) {
                // insert failed, check override method
                MethodInfo oi = mtbl[name];
                method_class* overridden_method = oi.method;

                if (oi.cls == cls->get_name()) {
                    ctbl->semant_error(cls->get_filename(), method) << "Redefinition of method '" << name << "'" << endl;
                } else {
                    if (overridden_method->get_ret() == ret && overridden_method->get_args() == args) {
                        mtbl[name] = info;
                    } else {
                        ctbl->semant_error(cls->get_filename(), method) << "In redefined method '" << name
                            << "', signature is different from original method in class '" << oi.cls << "'" << endl;
                    }
                }
            }
        }
    }
}

ClassInfo* ClassTable::get_cls_info(Symbol cls, Symbol s) {
    if (s == SELF_TYPE) {
        return tbl[cls];
    }
    auto info = tbl.find(s);
    if (info != tbl.end()) {
        return (*info).second;
    } else {
        return nullptr;
    }
}

void ClassTable::build_mtbl() {
    for(auto iter = tbl.begin(); iter != tbl.end(); ++iter){
        iter->second->build_mtbl(this);
    }
}

void ClassTable::check_main() {
    auto main = tbl.find(Main);
    if (main != tbl.end()) {
        auto mtbl = (*main).second->mtbl;
        auto meth = mtbl.find(main_meth);
        if (meth != mtbl.end()) {
            if ((*meth).second.method->get_args().size() != 0) {
                semant_error((*main).second->cls->get_filename(), (*meth).second.method)
                    << "Too many arguments to method 'main'" << endl;
            }
        } else {
            semant_error((*main).second->cls) << "Method main is not defined." << endl;
        }
    } else {
        semant_error() << "Class Main is not defined." << endl;
    }
}

bool ClassTable::is_subclass(Symbol cls, Symbol sub, Symbol parent){
    if (sub == No_type) return true;
    if (parent == SELF_TYPE) {
        // T <= SELF_TYPE(C) is always false when T is not SELF_TYPE
        if (sub == SELF_TYPE) return true;
        else return false;
    }
    if (sub == SELF_TYPE) sub = cls;
    
    Symbol p = sub;
    while (p != No_class) {
        if (p == parent) {
            return true;
        }
        p = tbl[p]->cls->get_parent();
    }
    return false;
}

Symbol ClassTable::find_lca(Symbol cls, Symbol a, Symbol b) {
    if (a == SELF_TYPE) a = cls;
    if (b == SELF_TYPE) b = cls;

    std::vector<Symbol> ancestor_a;
    std::vector<Symbol> ancestor_b;
    while (a != No_class) {
        ancestor_a.push_back(a);
        a = tbl[a]->cls->get_parent();
    }
    while (b != No_class) {
        ancestor_b.push_back(b);
        b = tbl[b]->cls->get_parent();
    }

    Symbol ca;
    auto ita = ancestor_a.rbegin();
    auto itb = ancestor_b.rbegin();
    while (ita != ancestor_a.rend() && itb != ancestor_b.rend() && *ita == *itb) {
        ca = *ita;
        ita++;
        itb++;
    }
    return ca;
}

void program_class::semant()
{
    initialize_constants();

    ClassTable *classtable = new ClassTable(classes);
    classtable->check_inheritance();

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    // build mtbl and check method signature
    classtable->build_mtbl();

    classtable->check_main();

    SymbolTable<Symbol, Symbol> *stbl = new SymbolTable<Symbol, Symbol>();

    ClassInfo* exit_flag = nullptr;
    std::stack<ClassInfo*> st;
    st.push(classtable->get_cls_info(No_class, Object));
    while (!st.empty()) {
        ClassInfo* cur = st.top();
        st.pop();

        if (cur == exit_flag) {
            stbl->exitscope();
        } else {
            stbl->enterscope();
            cur->cls->semant(classtable, stbl);
            st.push(exit_flag);
            auto children = cur->children;
            for (auto child : children) {
                st.push(child);
            }
        }
    }
    
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void class__class::semant(ClassTable *ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    // cout << "In class " << name << endl;
    stbl->addid(self, &SELF_TYPE);
    
    // add attr definition into scope
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        attr_class* attr = dynamic_cast<attr_class*>(feature);
        if (attr) {
            attr->semant(this ,ctbl, stbl);
        }
    }

    // check methods
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        method_class* method = dynamic_cast<method_class*>(feature);
        if (method) {
            method->semant(this ,ctbl, stbl);
        }
    }
}

void attr_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    if (ctbl->get_cls_info(cls->get_name(), type_decl)) {
        if (stbl->lookup(name)) {
            ctbl->semant_error(cls->get_filename(), this) << "Redefinition of attr '" << name << "'" << endl;
        } else {
            stbl->addid(name, &type_decl);
        }
    } else {
        if (type_decl != prim_slot) {
            ctbl->semant_error(cls->get_filename(), this) << "Undefined type '" << type_decl << "'" << endl;
        }
    }
    
    init->semant(cls, ctbl, stbl);
    Symbol e = init->get_type();
    if (!ctbl->is_subclass(cls->get_name(), e, type_decl) && !(e == SELF_TYPE && type_decl == cls->get_name())) {
        ctbl->semant_error(cls->get_filename(), this) << "Type mismatched, expected '" << type_decl
            << "', found '" << e << "'" << endl;
    }
}

void method_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    stbl->enterscope();
    // cout << "In method " << name << endl;
    // add formal attribute into scope
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal f = formals->nth(i);
        f->semant(cls, ctbl, stbl);
    }

    expr->semant(cls, ctbl, stbl);

    Symbol e = expr->get_type();

    if (!ctbl->is_subclass(cls->get_name(), e, return_type)) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid conversion from '" << e
                << "' to '" << return_type << "'" << endl;
    }
    stbl->exitscope();
}

void formal_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    if (ctbl->get_cls_info(cls->get_name(), type_decl)) {
        if (name == self) {
            ctbl->semant_error(cls->get_filename(), this) << "Cannot use 'self' as argument name" << endl;
        } else {
            if (stbl->probe(name)) {
                ctbl->semant_error(cls->get_filename(), this) << "Redefined argument '" << name << "'" << endl;
            } else {
                if (type_decl == SELF_TYPE) {
                    ctbl->semant_error(cls->get_filename(), this) << "Formal parameter '"
                        << name << "'cannot have type SELF_TYPE." << endl;
                }
                stbl->addid(name, &type_decl);
            }
        }
    } else {
        ctbl->semant_error(cls->get_filename(), this) << "Undefined type '" << type_decl << "'" << endl;
    }
}

void branch_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    stbl->enterscope();
    if (ctbl->get_cls_info(cls->get_name(), type_decl)) {
        stbl->addid(name, &type_decl);
        expr->semant(cls, ctbl, stbl);
    } else {
        ctbl->semant_error(cls->get_filename(), this) << "Undefined type '" << type_decl << "'" << endl;
    }
    stbl->exitscope();
}

void assign_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = No_type;
    expr->semant(cls, ctbl, stbl);
    Symbol e = expr->get_type();

    if (name == self) {
        ctbl->semant_error(cls->get_filename(), this) << "Cannot assign to 'self'" << endl;
    }

    Symbol* id = stbl->lookup(name);
    if (id) {
        if (ctbl->is_subclass(cls->get_name(), e, *id)) {
            type = e;
        } else {
            ctbl->semant_error(cls->get_filename(), this) << "Invalid conversion from '" << e
                << "' to '" << *id << "'" << endl;
        }
    } else {
        ctbl->semant_error(cls->get_filename(), this) << "'" << name << "' is undeclared in this scope" << endl;
    }
}

void static_dispatch_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    expr->semant(cls, ctbl, stbl);
    Symbol e = expr->get_type();

    type = No_type;
    if (ctbl->is_subclass(cls->get_name(), e, type_name)) {
        ClassInfo* cinfo = ctbl->get_cls_info(cls->get_name(), type_name);
        if (cinfo) {
            auto it = cinfo->mtbl.find(name);
            if (it != cinfo->mtbl.end()) {
                method_class* method = it->second.method;
                type = method->get_ret();
                if (type == SELF_TYPE) {
                    type = type_name;
                }

                Formals formals = method->get_formals();
                int al = actual->len();
                int fl = formals->len();
                if (al != fl) {
                    if (al > fl){
                        ctbl->semant_error(cls->get_filename(), this) << "Too many arguments to method '" << name << "'" << endl;
                    } else {
                        ctbl->semant_error(cls->get_filename(), this) << "Too few arguments to method '" << name << "'" << endl;
                    }
                } else {
                    for (int i = 0; i < al; i++) {
                        Expression expr = actual->nth(i);
                        expr->semant(cls, ctbl, stbl);
                        Symbol at = expr->get_type();
                        Symbol ft = formals->nth(i)->get_type();
                        if (!ctbl->is_subclass(cls->get_name(), at, ft)) {
                            ctbl->semant_error(cls->get_filename(), this) << "Type mismatched, expected '" << ft
                                << "', found '" << at << "'" << endl;
                        }
                    }
                }
            }
        } else {
            ctbl->semant_error(cls->get_filename(), this) << "Undefined type '" << type_name << "'" << endl;
        }
    } else {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid conversion from '" << e
            << "' to '" << type_name << "'" << endl;
    }
}

void dispatch_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = No_type;
    expr->semant(cls, ctbl, stbl);
    auto mtbl = ctbl->get_cls_info(cls->get_name(), expr->get_type())->mtbl;
    auto it = mtbl.find(name);
    if (it != mtbl.end()) {
        method_class* method = it->second.method;
        type = method->get_ret();
        if (type == SELF_TYPE) {
            type = expr->get_type();
        }

        Formals formals = method->get_formals();
        int al = actual->len();
        int fl = formals->len();
        if (al != fl) {
            if (al > fl){
                ctbl->semant_error(cls->get_filename(), this) << "Too many arguments to method '" << name << "'" << endl;
            } else {
                ctbl->semant_error(cls->get_filename(), this) << "Too few arguments to method '" << name << "'" << endl;
            }
        } else {
            for (int i = 0; i < al; i++) {
                Expression expr = actual->nth(i);
                expr->semant(cls, ctbl, stbl);
                Symbol at = expr->get_type();
                Symbol ft = formals->nth(i)->get_type();
                if (!ctbl->is_subclass(cls->get_name(), at, ft)) {
                    ctbl->semant_error(cls->get_filename(), this) << "Type mismatched, expected '" << ft
                        << "', found '" << at << "'" << endl;
                }
            }
        }
    } else {
        ctbl->semant_error(cls->get_filename(), this) << "Undefined method '" << name << "'" << endl;
    }
}

void cond_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    pred->semant(cls, ctbl, stbl);
    then_exp->semant(cls, ctbl, stbl);
    else_exp->semant(cls, ctbl, stbl);

    if (pred->get_type() != Bool) {
        type = No_type;
        ctbl->semant_error(cls->get_filename(), this) << "Type mismatched, expected 'Bool', found '"
            << pred->get_type() << "'" << endl;
    } else {
        type = ctbl->find_lca(cls->get_name(), then_exp->get_type(), else_exp->get_type());
    }
}

void loop_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = Object;
    pred->semant(cls, ctbl, stbl);
    body->semant(cls, ctbl, stbl);

    Symbol t = pred->get_type();
    if (t != Bool) {
        ctbl->semant_error(cls->get_filename(), this) << "Type mismatched, expected 'Bool', found '"
            << t << "'" << endl;
    }
}

void typcase_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = No_type;
    expr->semant(cls, ctbl, stbl);
    std::set<Symbol> cts;
    std::set<Symbol> ret_ty;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Case c = cases->nth(i);
        c->semant(cls, ctbl, stbl);
        if (!cts.insert(c->get_type()).second) {
            ctbl->semant_error(cls->get_filename(), this) << "Duplicate branch '"
                << c->get_type() << "' in case statement." << endl;
        }
        ret_ty.insert(c->get_ret_type());
    }

    auto it = ret_ty.begin();
    Symbol anc = *it;
    it++;
    while (it != ret_ty.end()) {
        anc = ctbl->find_lca(cls->get_name(), anc, *it);
        it++;
    }
    type = anc;
}

void block_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        Expression expr = body->nth(i);
        expr->semant(cls, ctbl, stbl);
    }
    type = body->nth(body->len() - 1)->get_type();
}

void let_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    stbl->enterscope();
    init->semant(cls, ctbl, stbl);
    Symbol init_type = init->get_type();
    if (!ctbl->is_subclass(cls->get_name(), init_type, type_decl)) {
        ctbl->semant_error(cls->get_filename(), this) << "Type mismatched, expected '"
            << type_decl << "', found '" << init_type << endl;
    }

    if (identifier == self) {
        ctbl->semant_error(cls->get_filename(), this) << "Cannot use 'self' as argument name" << endl;
    } else {
        stbl->addid(identifier, &type_decl);
    }

    body->semant(cls, ctbl, stbl);
    type = body->get_type();
    stbl->exitscope();
}

void plus_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    e2->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    Symbol t2 = e2->get_type();
    if (t1 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t1
            << "', expected 'Int'" << endl;
    }
    if (t2 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t2
            << "', expected 'Int'" << endl;
    }
    type = Int;
}

void sub_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    e2->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    Symbol t2 = e2->get_type();
    if (t1 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t1
            << "', expected 'Int'" << endl;
    }
    if (t2 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t2
            << "', expected 'Int'" << endl;
    }
    type = Int;
}

void mul_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    e2->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    Symbol t2 = e2->get_type();
    if (t1 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t1
            << "', expected 'Int'" << endl;
    }
    if (t2 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t2
            << "', expected 'Int'" << endl;
    }
    type = Int;
}

void divide_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    e2->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    Symbol t2 = e2->get_type();
    if (t1 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t1
            << "', expected 'Int'" << endl;
    }
    if (t2 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t2
            << "', expected 'Int'" << endl;
    }
    type = Int;
}

void neg_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    if (t1 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t1
            << "', expected 'Int'" << endl;
    }
    type = Int;
}

void lt_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    e2->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    Symbol t2 = e2->get_type();
    if (t1 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t1
            << "', expected 'Int'" << endl;
    }
    if (t2 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t2
            << "', expected 'Int'" << endl;
    }
    type = Bool;
}

void eq_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    e2->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    Symbol t2 = e2->get_type();
    if (t1 == Int || t1 == Str || t1 == Bool || t2 == Int || t2 == Str || t2 == Bool) {
        if (t1 != t2) {
            ctbl->semant_error(cls->get_filename(), this) << "Type mismatched for '" << t1
                << "' and '" << t2 << "'" << endl;
        }
    }
    type = Bool;
}

void leq_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    e2->semant(cls, ctbl, stbl);
    Symbol t1 = e1->get_type();
    Symbol t2 = e2->get_type();
    if (t1 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t1
            << "', expected 'Int'" << endl;
    }
    if (t2 != Int) {
        ctbl->semant_error(cls->get_filename(), this) << "Invalid type '" << t2
            << "', expected 'Int'" << endl;
    }
    type = Bool;
}

void comp_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    type = Bool;
}

void int_const_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = Int;
}

void bool_const_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = Bool;
}

void string_const_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = Str;
}

void new__class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = type_name;
}

void isvoid_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    e1->semant(cls, ctbl, stbl);
    type = Bool;
}

void no_expr_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    type = No_type;
}

void object_class::semant(Class_ cls, ClassTable* ctbl, SymbolTable<Symbol, Symbol>* stbl) {
    Symbol* id = stbl->lookup(name);
    if (id) {
        type = *id;
    } else {
        type = No_type;
        ctbl->semant_error(cls->get_filename(), this) << "'" << name << "' is undeclared in this scope" << endl;
    }
}

Symbol class__class::get_name() {
    return name;
}

Symbol class__class::get_parent() {
    return parent;
}

Features class__class::get_features() {
    return features;
}

Symbol method_class::get_name() {
    return name;
}

Symbol method_class::get_ret() {
    return return_type;
}

Formals method_class::get_formals() {
    return formals;
}

std::vector<Symbol> method_class::get_args() {
    std::vector<Symbol> args;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal f = formals->nth(i);
        args.push_back(f->get_type());
    }
    return args;
}


Symbol attr_class::get_name() {
    return name;
}

Symbol formal_class::get_name() {
    return name;
}

Symbol formal_class::get_type() {
    return type_decl;
}

Symbol branch_class::get_type() {
    return type_decl;
}

Symbol branch_class::get_ret_type() {
    return expr->get_type();
}
