// B2CMain.cpp
#include <iostream>
#include <map>
#include <stack>
#include <vector>
#include <string>
#include <functional>
#include <set>
#include "antlr4-runtime.h"
#include "antlr4-cpp/BBaseVisitor.h"
#include "antlr4-cpp/BLexer.h"
#include "antlr4-cpp/BParser.h"

using namespace std;
using namespace antlr4;
using namespace antlr4::tree;

enum Types {tyAUTO, tyINT, tyDOUBLE, tySTRING, tyBOOL, tyCHAR, tyFUNCTION, tyVOID, tyUNDEFINED, tyANY};
string mnemonicTypes[] = {"auto", "int", "double", "string", "bool", "char", "function", "void", "auto", "auto"};

struct SymbolAttributes {
   Types type; // int, double, bool, char, string, function --- auto if unknown yet
   // if type == "function"
   vector<Types> retArgTypes; // first element is a return_type
   vector<string> argName = vector<string>();
   bool declFunc = false;
   //bool undefined = 0;
};

class SymbolTable {
private:
    map<string, SymbolAttributes> table;  // symbol-name: string, symbol-typeInfo: SymbolAttributes

public:
    // Add a new symbol 
    void addSymbol(const string& name, const SymbolAttributes& attributes) {
		table[name] = attributes; 
    }

    // Check if a symbol exists
    bool symbolExists(const string& name) const {
        return table.find(name) != table.end();
    }

	void changeSymbolName(const string& oldName, const string& newName) {
		if(!symbolExists(oldName)) return;
		addSymbol(newName, getSymbolAttributes(oldName));
		removeSymbol(oldName);
	}

    // Get attributes of a symbol
    SymbolAttributes getSymbolAttributes(const string& name) const {
        if (symbolExists(name)) {
            return table.at(name);
        } else {
            cout << "Error: Symbol " << name << " not found" << endl;
        }
    }

    // Remove a symbol from the table
    void removeSymbol(const string& name) {
        table.erase(name);
    }

	map<string, SymbolAttributes>& getAllSymbol() {
		return table;
	}

    // Print all symbols in the table (for debugging purposes)
    void printSymbols() const {
        for (const auto& pair : table) {
            cout << "(name) " << pair.first << ", (type) " << mnemonicTypes[pair.second.type];
			if (pair.second.type == tyFUNCTION) {
				cout << " | ";
				int n = pair.second.retArgTypes.size();
				if (n > 0) {
					cout << mnemonicTypes[pair.second.retArgTypes[0]] << "("; // return type
				}
				for (int i = 1; i < n-1; i++)
					cout << mnemonicTypes[pair.second.retArgTypes[i]] << ", ";
				if (n > 1) {
					cout << mnemonicTypes[pair.second.retArgTypes[n-1]]; // last arg type
				}
				cout << ")";
			} 
	    	cout << endl;
        }
		cout << endl;
    }
};

/*
 * STEP 1. build symbol table
 */
const string _GlobalFuncName_ = "$_global_$";

// collection of per-function symbol tables accessed by function name
// symbol table in global scope can be accessed with special name defined in _GlobalFuncName_
map<string, SymbolTable*> symTabs;
void clearSymTabs() {
	for(auto& x : symTabs) {
		delete x.second;
	}
	symTabs.clear();
}

class SymbolTableVisitor : public BBaseVisitor {
private:
   vector<int> scopeLevel;
   string curFuncName;
   string curScopeName;

	//-2 for LastScope, -1 for Global, 0 for Func, 1 for Func_N
	string fullScope(int n = -2) {
		if(n == -2) n = scopeLevel.size();
		else if(n == -1) return _GlobalFuncName_;
		else if(n == 0) return curFuncName;

		string ret = curFuncName + "_$";
		for(int i = 0; i < n && i < scopeLevel.size(); ++i) {
			ret += to_string(scopeLevel[i]) + "_";
		}
		ret.pop_back();
		return ret;
	}
public:
	// Building symbol tables by visiting tree
	
	any visitProgram(BParser::ProgramContext *ctx) override {
		scopeLevel.clear();
		scopeLevel.push_back(0);
		//scopeLevel = 0; // global scope	
			
		// prepare symbol table for global scope
		SymbolTable* globalSymTab = new SymbolTable();
		curScopeName = curFuncName = _GlobalFuncName_;
    	symTabs[curScopeName] = globalSymTab;

		// visit children
    	for (int i=0; i< ctx->children.size(); i++) {
    		visit(ctx->children[i]);
    	}

		// print all symbol tables
		for (auto& pair : symTabs) {
	    	cout << "--- Symbol Table --- " << pair.first << endl; // function name
	    	pair.second->printSymbols();					   // per-function symbol table
			cout << "";
		}

    	return nullptr;
	}

    any visitDefinition(BParser::DefinitionContext *ctx) override {
		visit(ctx->children[0]);
        return nullptr;
	}

    any visitAutostmt(BParser::AutostmtContext *ctx) override {
    	// get current symbol table
		SymbolTable *stab = symTabs[curScopeName];

		// You can retrieve the variable names and constants using ctx->name(i) and ctx->constant(i)
		for (int i=0, j=0; i < ctx->name().size(); i++) {
			
			string varName = ctx->name(i)->getText();
			if(stab->symbolExists(varName)) {
				int lineNum = ctx->getStart()->getLine();
				cerr << endl << "[ERROR] visitAutostmt: Duplicate Variable Names in line " << lineNum << " --" << varName << endl;
				exit(-1); // error
			}
			enum Types varType = tyAUTO;				// default type

			// if initialized, get constant type
			int idx_assn = 1 + i*2 + j*2 + 1;  // auto name (= const)?, name (= const)?, ...
			if (ctx->children[idx_assn]->getText().compare("=") == 0) { 
				if (ctx->constant(j)) {  
					varType = any_cast<Types>( visit(ctx->constant(j)) );   // returns init constant type
					j++;
				}
			}

			stab->addSymbol(varName, {varType});
		}

    	return nullptr;
    }

    any visitConstant(BParser::ConstantContext *ctx) override {
        
		if (ctx->INT()) return tyINT;
		else if (ctx->REAL()) return tyDOUBLE;
		else if (ctx->STRING()) return tySTRING;
		else if (ctx->BOOL()) return tyBOOL;
		else if (ctx->CHAR()) return tyCHAR;

		cout << "[ERROR] unrecognizable constant is used for initialization: " << ctx->children[0]->getText() << endl;
		exit(-1);
        return nullptr;
    }

	any visitDeclstmt(BParser::DeclstmtContext *ctx) override {
		SymbolAttributes funcat;
		funcat.type = tyFUNCTION;
		funcat.retArgTypes.push_back(tyAUTO);

        string functionName = ctx->name()->getText();
        
		// You can retrieve and visit the parameter type list
		for (int i=1; i < ctx->AUTO().size(); i++) {
			funcat.retArgTypes.push_back(tyAUTO);	
		}

		if(symTabs[_GlobalFuncName_]->symbolExists(functionName)) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitDeclStmt: Duplicate function Declaration or Same name of Variable exists in line " << lineNum << " --" << functionName << endl;
			exit(-1);
		}
		
		SymbolTable *gstab = symTabs[_GlobalFuncName_];
		SymbolTable *stab = symTabs[curScopeName];
		gstab->addSymbol(functionName, funcat);
		stab->addSymbol(functionName, funcat);
        return nullptr;
    }

	any visitFuncdef(BParser::FuncdefContext *ctx) override {
		enum Types varType = tyAUTO;
		enum Types funcType = tyFUNCTION;

		vector<Types> curFuncRetArgs;
		curFuncRetArgs.push_back(varType);

		for(int i = 1; i < ctx->name().size(); ++i) {
			curFuncRetArgs.push_back(varType);
		}

		string fName = ctx->name(0)->getText();
		if(symTabs.find(fName) != symTabs.end()) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitFuncdef: Duplicate Function in line " << lineNum << " --" << fName << endl;
			exit(-1);
		}
		else if(!symTabs[curScopeName]->symbolExists(fName)) {
			symTabs[curScopeName]->addSymbol(fName, {funcType, curFuncRetArgs});
		}
		else if(symTabs[curScopeName]->getSymbolAttributes(fName).retArgTypes != curFuncRetArgs) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitFuncdef: Do not match previous function Declaration or Same name of Variable exists in line " << lineNum << " --" << fName << endl;
			exit(-1);
		}

		SymbolTable* funcSymTab = new SymbolTable();
		curFuncName = ctx->name(0)->getText();
		curScopeName = "";

		for(int i = 1; i < ctx->name().size(); ++i) {
			funcSymTab->addSymbol(ctx->name(i)->getText(), {varType});
		}

		scopeLevel.clear();
		scopeLevel.push_back(0);

		symTabs[curFuncName] = funcSymTab;
		visit(ctx->blockstmt());
		return nullptr;
    }

	any visitBlockstmt(BParser::BlockstmtContext *ctx) override {
		if(curScopeName == "") curScopeName = curFuncName;
		else {
			(*scopeLevel.rbegin())++;
			
			curScopeName = fullScope();

			SymbolTable* scopeSymTab = new SymbolTable();
			symTabs[curScopeName] = scopeSymTab;
			scopeLevel.push_back(0);
		}

    	for (auto stmt : ctx->statement()) {
      	    visit(stmt);
    	}

		scopeLevel.pop_back();
		if(scopeLevel.empty()) curFuncName = curScopeName = _GlobalFuncName_;
		else {
			curScopeName = fullScope(scopeLevel.size()-1);
		}

    	return nullptr;
    }

    any visitDirective(BParser::DirectiveContext *ctx) override {
		visit(ctx->SHARP_DIRECTIVE());
        return nullptr;
	}

	any visitStatement(BParser::StatementContext *ctx) override {
		visit(ctx->children[0]);
        return nullptr;
    }

	any visitIfstmt(BParser::IfstmtContext *ctx) override {
		visit(ctx->expr());
		visit(ctx->statement(0));
		if (ctx->ELSE()) {
	   		visit(ctx->statement(1));
		}
        return nullptr;
    }
	
    any visitWhilestmt(BParser::WhilestmtContext *ctx) override {
        visit(ctx->expr());
        visit(ctx->statement());
        return nullptr;
    }

    any visitExpressionstmt(BParser::ExpressionstmtContext *ctx) override {
		visit(ctx->expression());
        return nullptr;
    }

	any visitReturnstmt(BParser::ReturnstmtContext *ctx) override {
		if (ctx->expression()) {
			visit(ctx->expression());
		}
        return nullptr;
    }

    any visitNullstmt(BParser::NullstmtContext *ctx) override {
        return nullptr;
    }
};

/*
 * STEP 2. infer type
 */   

struct dt {
	string scope;
	string var;
	int arg = 0;

	bool operator<(const dt &d) const {
		if(scope != d.scope) return scope < d.scope;
		if(var != d.var) return var < d.var;
		return arg < d.arg;
	}

	bool operator==(const dt&d) const {
		return scope == d.scope && var == d.var && arg == d.arg;
	}
};

struct pdt {
	dt s1;
	dt s2;
	char op;
};

struct t {
	int destidx;
	dt src;
	vector<pdt> prereq;
	int line;
};

class DefineType {
private:
	vector<pair<dt, vector<int>>> dest; //prereq인 un이 정해지면 dest가 정해짐. 저장하는 값은 정해져야 하는 값들 중 un의 index
	map<dt, int> destIdx; //dt를 통해서 dest의 idx을 찾음
	vector<t> un; //
	vector<bool> checkUn;

public:
	void addUndefined(dt& ddt, dt src, vector<pdt>& prereq, int line) {	
		if(destIdx.count(ddt) <= 0) {
			destIdx[ddt] = dest.size();
			dest.push_back(make_pair(ddt, vector<int>()));
		}

		dest[destIdx[ddt]].second.push_back(un.size());
		un.push_back({destIdx[ddt], src, prereq, line});
		prereq.clear();
	}

	void addUndefined(dt src, vector<pdt>& prereq, int line, int destIdx) {	
		un.push_back({destIdx, src, prereq, line});
		prereq.clear();
	}

	Types getType(dt a) {
		if(a.scope == "") return static_cast<Types>(a.arg);
		SymbolAttributes sa = symTabs[a.scope]->getSymbolAttributes(a.var);
		if(sa.type != tyFUNCTION) return sa.type;
		else return sa.retArgTypes[0];
	}

	dt emptydt(Types t) {
		dt a;
		a.scope = a.var = "";
		a.arg = t;
		return a;
	}

	dt emptydt(int t) {
		dt a;
		a.scope = a.var = "";
		a.arg = t;
		return a;
	}

	void changeDestStr(string& func, string oldVar, string newVar, int arg) {
		dt d1 = {func, oldVar, arg};
		if(destIdx.find(d1) == destIdx.end()) return;

		int i = destIdx[d1];
		dt d2 = {func, newVar, arg};
		dest[i].first.var = newVar;
		destIdx[d2] = i;
		destIdx.erase(d1);
	}

	void show() {
		cout << "-----Dest:" << endl;
		cout << "[i\tVar\tdest]" << endl;
		for(int i = 0; i < dest.size(); ++i) {
			cout << i << '\t' << dest[i].first.var << '\t';
			for(auto x : dest[i].second) {
				cout << x << ' ';
			}
			cout << endl;
		}
		cout << "-----destIdx:" << endl;
		cout << "[scope\tvar\targ\tdestIdx]" << endl;
		for(auto x : destIdx) {
			cout << x.first.scope << '\t' << x.first.var << '\t' << x.first.arg << '\t' << x.second << endl;
		}
		cout << "-----un:" << endl;
		cout << "[Idx\tline\tdestIdx\tsrc.scp\tsrc.var\tsrc.arg]" << endl;

		for(int i = 0; i < un.size(); ++i) {
			auto x = un[i];
			cout << i << '\t' << x.line << '\t' << x.destidx << '\t' << x.src.scope << '\t' << x.src.var << '\t' << x.src.arg << endl;
			for(auto u : x.prereq) {
				cout << u.s1.scope << '\t' << u.s1.var << '\t' << u.s1.arg << '\t';
				cout << u.s2.scope << '\t' << u.s2.var << '\t' << u.s2.arg << '\t' << u.op << endl;
			}
			cout << "---" << endl;
		}
	}

	int setPrereq(t& now) {
		int ret = 2;
		vector<pdt>& prereq = now.prereq;
		if(prereq.empty()) return -1;

		for(auto it = prereq.begin(); it != prereq.end(); ++it) {
			Types ta = getType(it->s1), tb = getType(it->s2);
			if(ta == tyUNDEFINED || tb == tyUNDEFINED) {
				continue;
			}
			else if(ta == tyVOID || tb == tyVOID) {
				if(it->op != 'v') {
					cerr << endl << "[ERROR] setPrereq: void types cannot operate in line " << now.line << " --" << it->s1.var << ", " << it->s2.var << endl;
					exit(-1);
				}
				else if(ta != tb) {
					cerr << endl << "[ERROR] setPrereq: type does not match in line " << now.line << " --" << mnemonicTypes[ta] << ", " << mnemonicTypes[tb] << endl;
					exit(-1);
				}
			}
			
			if(it->s1 == it->s2 || ta == tb) {
				ret = 1;
				prereq.erase(it);
				--it;
			}
			else if(ta == tyANY || tb == tyANY) {
				Types& t = ta != tyANY ? ta : tb;

				if(t == tyUNDEFINED) continue;
				else if(ta != tyVOID) {
					ret = 1;
					prereq.erase(it);
					--it;
				}
				else {	
					cerr << endl << "[ERROR] setPrereq: In tenary operator, condition is void type in line " << now.line << " --" << it->s1.var << endl;
					exit(-1);
				}
			}
			else if(ta != tyUNDEFINED && tb != tyUNDEFINED) {
				cerr << endl << "[ERROR] setPrereq: types does not match in line " << now.line << " --" << mnemonicTypes[ta] << ", " << mnemonicTypes[tb] << endl;
				exit(-1);
			}
		}

		return prereq.empty() ? 0 : ret;
	}

	int inferType() {
		int ret = 0;
		for(int i = 0; i < un.size(); ++i) {

			if(checkUn[i]) continue;
			int c = setPrereq(un[i]);

			switch(c) {
			case 2:
				continue;
			case 1:
				ret = 1;
				continue;
			case 0:
				ret = 1;
			}

			if(un[i].destidx == -1) {
				Types type = getType(un[i].src);
				if(type == tyUNDEFINED) continue;
				checkUn[i] = true;
				return 1;
			}
			else if(un[i].destidx == -2) {
				Types type = getType(un[i].src);
				if(type == tyUNDEFINED) continue;
				else if(type == tyVOID) {
					cerr << endl << "[ERROR] inferType: void type cannot assigned in macro function parameter in line " << un[i].line << endl;
					exit(-1);
				}
				checkUn[i] = true;
				return 1;
			}

			dt ass = dest[un[i].destidx].first;
			dt val = un[i].src;
			bool assFunc = symTabs[_GlobalFuncName_]->symbolExists(ass.var) && symTabs[_GlobalFuncName_]->getSymbolAttributes(ass.var).type == tyFUNCTION;
			bool valFunc = symTabs[_GlobalFuncName_]->symbolExists(val.var) && symTabs[_GlobalFuncName_]->getSymbolAttributes(val.var).type == tyFUNCTION;

			Types assType = getType(ass);
			Types valType = getType(val);

			if(valType == tyUNDEFINED) continue;
			else if(assFunc && valFunc && valType == tyVOID) {
				cerr << endl << "[ERROR] inferType: void type cannot returned in line " << un[i].line << " --" << ass.var << endl;
				exit(-1);
			}
			else if(!assFunc && valType == tyVOID) {
				cerr << endl << "[ERROR] inferType: void type cannot assigned in variable in line " << un[i].line << " --" << ass.var << endl;
				exit(-1);
			}
			else if(valType == tyANY) {
				if(assType == tyANY) continue;
				valType = assType;
			}
			else if(assType != valType && assType != tyAUTO && assType != tyUNDEFINED) {
				if(un[i].line == -1) {
					cerr << endl << "[ERROR] inferType: Function ended with non return, which does not match before return" << " --" << ass.var << "()" << endl;
				}
				else if(symTabs[ass.scope]->getSymbolAttributes(ass.var).type == tyFUNCTION && ass.arg == 0) {
					cerr << endl << "[ERROR] inferType: return types does not match in line " << un[i].line << " --" << mnemonicTypes[getType(ass)] << ", " << mnemonicTypes[getType(val)] << endl;
				}
				else {
					cerr << endl << "[ERROR] inferType: types does not match in line " << un[i].line << " --" << mnemonicTypes[getType(ass)] << ", " << mnemonicTypes[getType(val)] << endl;
				}
				exit(-1);
			}

			checkUn[i] = true;

			if(assFunc) {
				SymbolAttributes sa = symTabs[_GlobalFuncName_]->getSymbolAttributes(ass.var);
				sa.retArgTypes[ass.arg] = valType;
				symTabs[_GlobalFuncName_]->addSymbol(ass.var, sa);
				return 1;
			}

			symTabs[ass.scope]->addSymbol(ass.var, {valType});
			if(ass.arg > 0) {
				SymbolAttributes sa = symTabs[_GlobalFuncName_]->getSymbolAttributes(ass.scope);
				sa.retArgTypes[ass.arg] = valType;
				symTabs[_GlobalFuncName_]->addSymbol(ass.scope, sa);
				return 1;
			}
		}

		return ret;
	}

	void makeSymbol() {
		checkUn.clear();
		checkUn.resize(un.size(), false);
		while(inferType()) {
			//cout << "***" << endl;
		}

		/*
		cout << endl << "--------Not Assigned Idx: " << endl;
		for(int i = 0; i < un.size(); ++i) {
			if(checkUn[i]) continue;
			cout << i << ' ';
			/*dt ass = dest[un[i].destidx].first;
			dt val = un[i].src;
			bool assFunc = symTabs[_GlobalFuncName_]->symbolExists(ass.var) && symTabs[_GlobalFuncName_]->getSymbolAttributes(ass.var).type == tyFUNCTION;
			bool valFunc = symTabs[_GlobalFuncName_]->symbolExists(val.var) && symTabs[_GlobalFuncName_]->getSymbolAttributes(val.var).type == tyFUNCTION;

			Types assType = getType(ass);
			Types valType = getType(val);

			cout << i << ' ' << assFunc << ' ' << valFunc << ' ' << mnemonicTypes[assType] << ' ' << mnemonicTypes[valType] << endl;
			for(auto x : un[i].prereq) {
				cout << x.s1.var << ' ' << x.s1.arg << ' ' << x.s2.var << ' ' << x.s2.arg << endl;
			}
		}
		cout << endl;*/

		for(auto s : symTabs) {
			for(auto v : s.second->getAllSymbol()) {
				if(v.second.type == tyAUTO || v.second.type == tyUNDEFINED || v.second.type == tyANY) {
					cerr << endl << "[ERROR] makeSymbol: variable type does not assigned, remained in auto --" << v.first << endl;
					exit(-1);
				}
				else if(v.second.declFunc) {
					cerr << endl << "[ERROR] makeSymbol: function has only prototype, no definition --" << v.first << endl;
					exit(-1);
				}
				else if(v.second.type == tyFUNCTION) {
					if(v.second.retArgTypes[0] == tyUNDEFINED || v.second.retArgTypes[0] == tyANY) {
						cerr << endl << "[ERROR] makeSymbol: function return value cannot inference, remained in auto --" << v.first << endl;
						exit(-1);
					}
					for(int i = 1; i < v.second.retArgTypes.size(); ++i) {
						if(v.second.retArgTypes[i] == tyUNDEFINED) {
							cerr << endl << "[ERROR] makeSymbol: function parameter does not assigned, remained in auto --" << v.first << " - " << v.second.argName[i] << endl;
							exit(-1);
						}
					}
				}
			}
		}

		/*for(int i = 0; i < 10; ++i) {
			int t = inferType();
			//cout << endl << t << "*"<< endl;
		}

		cout << endl << "--------Not Assigned Idx: " << endl;
		for(int i = 0; i < un.size(); ++i) {
			if(!checkUn[i]) cout << i << ' ';
		}
		cout << endl;*/
	}
};

//DefineType dftable;

class DirectiveAnalysis {
private:
	/*const string DIRECTIVE_STR[7] = {"#@o include@01o @06@o", 
					  			    "define@t @02@05f (@o @03@t @06@o",
								    "[a-zA-Z]",
								    ")@04n",
								    "@02@o @03o",
								    "@02@t ",
								    "ANYTEXT"};*/
	const string DIRECTIVE_STR[11] = {
		"#@o include@04o <@02n @01@n >@o", 	//0
		"TEXTUNTIL>@p",						//1
		"\"@n @03@n \"@o",					//2
		"TEXTUNTIL\"@p",					//3
		"define@t @05@07t @10@n",			//4
		"FUNCNAME@p",						//5
		"PARANAME@p",						//6
		"@05@f (@o @08@t @10@n",			//7
		")@09n",							//8
		"@06@o ,@08o @09@n",				//9
		"ANYTEXT@p"							//10
	};


	struct t {
		string text = "";
		int failureJmp = -1;
		int doJmp = 0;
		char spaces;
	};

	t nextSymbol(string& line, int& idx) {
		t ret;
		bool fir = true;
		//cout << "ST: " << idx << endl; 
		while(idx < line.length() && line[idx] != ' ') {
			if(line[idx] == '@') {
				//cout << "@" << idx << endl;
				if(isalpha(line[idx+1])) {
					ret.spaces = line[idx+1];
					ret.failureJmp = 99;
					idx += 3;
					//cout << "***" << ret.text << ' ' << ret.doJmp << ' ' << ret.failureJmp << ' ' << ret.spaces << endl;
					return ret;
				}

				int t = stoi(line.substr(idx+1, 2));
				if(fir) {
					//cout << "***" << endl;
					ret.doJmp = t;
					idx += 3;
					fir = false;
					continue;
				}
				
				//cout << "FULL" << endl;
				ret.failureJmp = t;
				ret.spaces = line[idx+3];
				idx += 5;
				return ret;
			}

			fir = false;
			ret.text += line[idx++];
		}

		//if(!ret.text.empty()) ret.failureJmp = 0;
		return ret;
	}

	vector<vector<t>> directive;
	void makeTable() {
		if(!directive.empty()) return;
		for(string line : DIRECTIVE_STR) {
			//cout << line << endl;
			directive.push_back(vector<t>());
			int idx = 0;
			t tmp;
			while((tmp = nextSymbol(line, idx)).failureJmp >= 0) {
				directive.rbegin()->push_back(tmp);
			}
		}
	}

	vector<vector<string>> ret;

	bool isSpace(char c) {
		return c == ' ' || c == '\n' || c == '\t' || c == '\r';
	}

	int checkSpace(string& line, int idx, char option) {
		switch(option) {
		case 'p':
		case 'n':
			return idx;
		case 'f':
			//cout << "F" << ' ' << idx << ' ' << (idx >= line.length() || !isSpace(line[idx])) << endl;
			if(idx >= line.length() || !isSpace(line[idx]))
				return idx;
			else
				return -1;
		case 't': {
			bool flag = false;
			while(idx < line.length() && isSpace(line[idx])) {
				++idx;
				flag = true;
				//cout << "T" << idx << endl;
			}
			if(!flag) return -1;
			else return idx;
		}
		case 'o':
			while(idx < line.length() && isSpace(line[idx])) ++idx;
			return idx;
		}

		return -1;
	}

	string capitalStmtRet;
	int capitalStmt(string& line, int idx, int st) {
		//cout << "CAPITAL START: " << idx << endl;
		string& now = directive[st][0].text;
		string str = "";
		capitalStmtRet = "";
		if(now == "TEXTUNTIL>") {
			while(idx < line.length() && line[idx] != '>') {
				str += line[idx++];
			}
			if(idx >= line.length()) return -1;
			
			capitalStmtRet = str;
			return idx;
		}
		else if(now == "TEXTUNTIL\"") {
			while(idx < line.length() && line[idx] != '\"') {
				str += line[idx++];
			}
			if(idx >= line.length()) return -1;
			
			capitalStmtRet = str;
			return idx;
		}
		else if(now == "FUNCNAME" || now == "PARANAME") {
			if(!(('a' <= line[idx] && line[idx] <= 'z') || ('A' <= line[idx] && line[idx] <= 'Z'))) return -1;
			str += line[idx++];

			while(idx < line.length() && (('a' <= line[idx] && line[idx] <= 'z') 
			|| ('A' <= line[idx] && line[idx] <= 'Z') || ('0' <= line[idx] && line[idx] <= '9') 
			|| line[idx] == '_')) {
				str += line[idx++];
			}

			capitalStmtRet = str;
			return idx;
		}
		else if(now == "ANYTEXT") {
			while(idx < line.length()) {
				str += line[idx++];
			}
			capitalStmtRet = str;
			return idx;
		}
		else return -1;
	}

	void diranalysisAdd(int st, int st2) {
		//cout << "ADD: [" << st << ' ' << st2 << "]" << endl;
		switch(st) {
		case 0:
			if(st2 == 1) ret[0].push_back("include");
			if(st2 == 3) ret[0].push_back(capitalStmtRet);
			if(st2 == 4) ret[0].push_back("<");
			return;
		case 2:
			if(st2 == 1) ret[0].push_back(capitalStmtRet);
			if(st2 == 2) ret[0].push_back("\"");
			return;
		case 4:
			if(st2 == 0) ret[0].push_back("define");
			if(st2 == 1) ret[0].push_back(capitalStmtRet);
			if(st2 == 2) ret[0].push_back(capitalStmtRet);
			return;
		case 7:
			if(st2 == 0) ret[0].push_back(capitalStmtRet);
			if(st2 == 1) ret.push_back(vector<string>());
			if(st2 == 3) ret[0].push_back(capitalStmtRet);
			return;
		case 9:
			if(st2 == 0) ret[1].push_back(capitalStmtRet);
			return;
		}
	}

	int diranalysis(string& line, int idx, int st){
		//cout << '[' << st << ']' << endl;
		if(st == 99) {
			return -1;
		}

		if(directive[st][0].spaces == 'p') {
			//cout << "CAPITAL" << endl;
			int r = capitalStmt(line, idx, st);
			//cout << capitalStmtRet << endl;
			return r;
		}

		for(int i = 0; i < directive[st].size(); ++i) {
			//cout << "[" << st << ' ' << i << "] " << "idx: " << idx << endl;
			t& now = directive[st][i];
			if(now.doJmp) {
				int t = diranalysis(line, idx, now.doJmp);
				if(t < 0) {
					//cout << "[" << st << ' ' << i << "] ";
					//cout << "FAIL0" << ' ' << now.failureJmp << endl;
					return diranalysis(line, idx, now.failureJmp);
				}

				t = checkSpace(line, t, now.spaces);
				//cout << "CS: " << t << endl;
				if(t < 0) {
					//cout << "[" << st << ' ' << i << "] ";
					//cout << "FAIL1" << ' ' << now.failureJmp << endl;
					return diranalysis(line, idx, now.failureJmp);
				}
				idx = t;
				diranalysisAdd(st, i);
				continue;
			}
			
			if(line.substr(idx, now.text.length()) != now.text) {
				//cout << "FAIL2" << ' ' << now.failureJmp << endl;
				return diranalysis(line, idx, now.failureJmp);
			}

			int t = checkSpace(line, idx + now.text.length(), now.spaces);
			if(t < 0) {
				//cout << "FAIL3" << ' ' << now.failureJmp << endl;
				return diranalysis(line, idx, now.failureJmp);
			}

			idx = t;
			diranalysisAdd(st, i);
		}

		return idx;
	}

public:
	vector<vector<string>> diranalysis(string& line){ 
		makeTable();
		ret.clear();
		ret.push_back(vector<string>());
		/*for(auto x : ret) {
			cout << "----" << endl;
			for(auto s : x) {
				cout << "*" << s << endl;
			}
		}*/

		int r = diranalysis(line, 0, 0);
		if(r < 0 || r != line.length()) {
			ret.clear();
		}
		//cout << "LL: " << r << endl;
		//ret.clear();
		//ret.push_back(vector<string>());
		return ret;
	}
};


class TypeAnalysisVisitor : public BBaseVisitor, public DefineType, DirectiveAnalysis {
private:
    vector<int> scopeLevel;
    string curFuncName;
    string curScopeName;
	bool FuncReturned, hiddenScope;

	bool haveHeader = false;
	set<string> macroVar;
	map<string, int> macroFunc;
	
	vector<pdt> exprPrereq;
	//dt tmpdt = {"", "", 0};

	//-2 for LastScope, -1 for Global, 0 for Func, 1 for Func_N
	string fullScope(int n = -2) {
		if(n == -2) n = scopeLevel.size();
		else if(n == -1) return _GlobalFuncName_;
		else if(n == 0) return curFuncName;

		string ret = curFuncName + "_$";
		for(int i = 0; i < n && i < scopeLevel.size(); ++i) {
			ret += to_string(scopeLevel[i]) + "_";
		}
		ret.pop_back();
		return ret;
	}

	string findSymbol(string s) {
		string ret = "";
		for(int i = scopeLevel.size() - 1; i >= -1; --i) {
			ret = fullScope(i);
			if(symTabs[ret]->symbolExists(s)) {
				return ret;
			}
		}
		
		/* TODO: Error Handling*/
		return "";
	}

	int AssignExpr(dt assign, pair<dt, bool> val, int lineNum, bool doAssign = true) {
		Types assType = getType(assign);
		Types valType = getType(val.first);
		bool uval = val.second;

		if(valType == tyAUTO) {
			return -1;
		}
		else if(assType == tyANY) {
			return -4;
		}
		else if(valType == tyANY && assType == tyAUTO) {
			return -3;
		}
		else if(valType != assType && assType != tyAUTO && assType != tyUNDEFINED && valType != tyANY && !uval) {
			return -2;
		}	

		else if(!uval && assType != tyUNDEFINED) {
			if(valType == tyANY) valType = assType;
			if(doAssign) symTabs[assign.scope]->addSymbol(assign.var, {valType});
			return valType;
		}

		//Assign이 UNDEFINED 상태임
		else {
			if(assType != tyAUTO && assType != tyUNDEFINED) {
				vector<pdt> tmp;
				addUndefined(assign, emptydt(assType), tmp, lineNum);
			}

			addUndefined(assign, val.first, exprPrereq, lineNum);
			if(doAssign) symTabs[assign.scope]->addSymbol(assign.var, {tyUNDEFINED});
			return tyUNDEFINED;
		}

		return 0;
	}

	int AssignExpr(pair<dt, bool> val, int lineNum, int destIdx = -1, bool ForceAssign = false) {
		Types valType = getType(val.first);
		bool uval = val.second;

		if(valType == tyAUTO) {
			return -1;
		}

		else if(!uval && valType != tyANY && !ForceAssign) {
			return 0;
		}

		else {
			addUndefined(val.first, exprPrereq, lineNum, destIdx);
		}

		return 0;
	}

	void addPrereq(dt a, dt b, char op = 'x') {
		exprPrereq.push_back({a, b, op});
	}

	vector<string> getText(string& line) {
		vector<string> ret;
		ret.push_back("");

		bool text = false;
		bool arg = false;
		bool pare = false;

		for(int i = 0; i < line.size(); ++i) {
			char c = line[i];

			if(text && (c == ' ' || c == '\t')) {
				if(!(*ret.rbegin()).empty()) ret.push_back("");
				continue;
			}

			if(text && (c == '>' || c == '\"')) text = false;
			if(arg && c == ')') pare = false;
			if(!arg && c == '(' && line[i-1] != ' ' && line[i-1] != '\t')

			if(c == '#' || c == '<' || c == '>' || c == '\"' || c == ',' || c == '(' || c == ')')
				if(!(*ret.rbegin()).empty()) ret.push_back("");

			*ret.rbegin() += c;
			
			if(c == '#' || c == '<' || c == '>' || c == '\"' || c == ',' || c == '(' || c == ')')
				if(!(*ret.rbegin()).empty()) ret.push_back("");
			
			if(!text && (c == '<' || c == '\"')) text = false;
		}

		return ret;
	}

	int macroFuncInvocation(BParser::FuncinvocationContext *ctx) {
		string funcName = ctx->name()->getText();
		if(macroFunc.count(funcName) <= 0) {
			return 0;
		}

		int argNum = macroFunc[funcName];

		if(argNum != ctx->expr().size()) {
			int lineNum = ctx->getStart()->getLine();
			cerr << "[Error] macroFuncInvocation: number of macro function arguments does not match in line " << lineNum << " --" << funcName << endl;
			exit(-1);
		}

		//cout << ctx->name()->getText() << "(";
		for (int i=0; i < ctx->expr().size(); i++) {
			pair<dt, bool> val = any_cast<pair<dt, bool>>(visit(ctx->expr(i)));
			int lineNum = ctx->getStart()->getLine();

			int c;
			switch(c = AssignExpr(val, lineNum, -2)) {
			case -1:
				cerr << endl << "[ERROR] macroFuncInvocation: cannot infer types of Macro Function Parameters by auto variable in line " << lineNum << " --" << funcName <<  endl;
				exit(-1);
			//case -2:
				//cerr << endl << "[ERROR] macroFuncInvocation: types does not match with Function Parameters in line " << lineNum << " --" << funcName << endl;
				//exit(-1);
			}
		}

		return 1;
	}

	int headerFuncInvocation(BParser::FuncinvocationContext *ctx) {
		if(!haveHeader) return 0;

		string funcName = ctx->name()->getText();

		//cout << ctx->name()->getText() << "(";
		for (int i=0; i < ctx->expr().size(); i++) {
			pair<dt, bool> val = any_cast<pair<dt, bool>>(visit(ctx->expr(i)));
			int lineNum = ctx->getStart()->getLine();

			int c;
			switch(c = AssignExpr(val, lineNum, -2)) {
			case -1:
				cerr << endl << "[ERROR] macroFuncInvocation: cannot infer types of Macro Function Parameters by auto variable in line " << lineNum << " --" << funcName <<  endl;
				exit(-1);
			//case -2:
				//cerr << endl << "[ERROR] macroFuncInvocation: types does not match with Function Parameters in line " << lineNum << " --" << funcName << endl;
				//exit(-1);
			}
		}

		return 1;
	}


public:
   // infer types for 'auto' variables and functions
   // ...
 	any visitProgram(BParser::ProgramContext *ctx) override {
		scopeLevel.clear();
		scopeLevel.push_back(0);

		haveHeader = hiddenScope = false;
		macroFunc.clear();
		macroVar.clear();
		//scopeLevel = 0; // global scope	
			
		// prepare symbol table for global scope
		SymbolTable* globalSymTab = new SymbolTable();
		curScopeName = curFuncName = _GlobalFuncName_;
    	symTabs[curScopeName] = globalSymTab;

		// visit children
    	for (int i=0; i< ctx->children.size(); i++) {
			//cout << ctx->children[i]->getText() << endl;
    		visit(ctx->children[i]);
    	}

		//show();
		makeSymbol();

		// print all symbol tables
		for (auto& pair : symTabs) {
	    	cout << "--- Symbol Table --- " << pair.first << endl; // function name
	    	pair.second->printSymbols();					   // per-function symbol table
			cout << "";
		}

    	return nullptr;
	}

	/*any visitProgram(BParser::ProgramContext *ctx) override {
		scopeLevel.clear();
		scopeLevel.push_back(0);
		//scopeLevel = 0; // global scope	
			
		// prepare symbol table for global scope

		curScopeName = curFuncName = _GlobalFuncName_;

		// visit children
    	for (int i=0; i< ctx->children.size(); i++) {
			cout << ctx->children[i]->getText() << endl;
    		visit(ctx->children[i]);
    	}

		// print all symbol tables
		for (auto& pair : symTabs) {
	    	cout << "--- Symbol Table --- " << pair.first << endl; // function name
	    	pair.second->printSymbols();					   // per-function symbol table
			cout << "";
		}

    	return nullptr;
	}*/

    any visitDefinition(BParser::DefinitionContext *ctx) override {
		visit(ctx->children[0]);
        return nullptr;
	}

	any visitAutostmt(BParser::AutostmtContext *ctx) override {
    	// get current symbol table
		SymbolTable *stab = symTabs[curScopeName];
		
		// You can retrieve the variable names and constants using ctx->name(i) and ctx->constant(i)
		for (int i=0, j=0; i < ctx->name().size(); i++) {
			string varName = ctx->name(i)->getText();
			if(stab->symbolExists(varName)) {
				int lineNum = ctx->getStart()->getLine();
				cerr << endl << "[ERROR] visitAutostmt: Duplicate Variable Names in line " << lineNum << " --" << varName << endl;
				exit(-1); // error
			}
			enum Types varType = tyAUTO;				// default type
			
			// if initialized, get constant type
			int idx_assn = 1 + i*2 + j*2 + 1;  // auto name (= const)?, name (= const)?, ...
			if (ctx->children[idx_assn]->getText().compare("=") == 0) { 
				if (ctx->constant(j)) {  
					varType = getType(any_cast<pair<dt, bool>>( visit(ctx->constant(j)) ).first);   // returns init constant type
					j++;
				}
			}

			stab->addSymbol(varName, {varType});
		}

    	return nullptr;
    }

    any visitConstant(BParser::ConstantContext *ctx) override {
		if (ctx->INT()) return make_pair(emptydt(tyINT), false);
		else if (ctx->REAL()) return make_pair(emptydt(tyDOUBLE), false);
		else if (ctx->STRING()) return make_pair(emptydt(tySTRING), false);
		else if (ctx->BOOL()) return make_pair(emptydt(tyBOOL), false);
		else if (ctx->CHAR()) return make_pair(emptydt(tyCHAR), false);

		//return make_pair(tmpdt, false);

		cout << "[ERROR] unrecognizable constant is used for initialization: " << ctx->children[0]->getText() << endl;
		exit(-1);
        return nullptr;
    }

	any visitDeclstmt(BParser::DeclstmtContext *ctx) override {
		SymbolAttributes funcat;
		funcat.type = tyFUNCTION;
		funcat.retArgTypes.push_back(tyUNDEFINED);
		funcat.argName.push_back("");
		funcat.declFunc = true;

        string functionName = ctx->name()->getText();
		symTabs[functionName] = new SymbolTable();

		// You can retrieve and visit the parameter type list
		for (int i=1; i < ctx->AUTO().size(); i++) {
			funcat.retArgTypes.push_back(tyUNDEFINED);
			funcat.argName.push_back(to_string(i));
			symTabs[functionName]->addSymbol(to_string(i), {tyUNDEFINED});
		}

		if(symTabs[_GlobalFuncName_]->symbolExists(functionName)) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitDeclStmt: Duplicate function Declaration or Same name of Variable exists in line " << lineNum << " --" << functionName << endl;
			exit(-1);
		}
		
		SymbolTable *gstab = symTabs[_GlobalFuncName_];
		gstab->addSymbol(functionName, funcat);

        return nullptr;
    }

	any visitFuncdef(BParser::FuncdefContext *ctx) override {
		bool declFuncExist = false;

		enum Types varType = tyUNDEFINED;
		enum Types funcType = tyFUNCTION;

		vector<Types> curFuncRetArgs;
		vector<string> curFuncArgName;
		
		curFuncRetArgs.push_back(tyUNDEFINED);
		curFuncArgName.push_back("");

		for(int i = 1; i < ctx->name().size(); ++i) {
			curFuncRetArgs.push_back(varType);
			curFuncArgName.push_back(ctx->name(i)->getText());
		}

		string fName = ctx->name(0)->getText();
		if(symTabs.find(fName) != symTabs.end() && !symTabs[_GlobalFuncName_]->getSymbolAttributes(fName).declFunc) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitFuncdef: Duplicate Function in line " << lineNum << " --" << fName << endl;
			exit(-1);
		}
		else if(symTabs[_GlobalFuncName_]->symbolExists(fName) && symTabs[_GlobalFuncName_]->getSymbolAttributes(fName).retArgTypes.size() != curFuncRetArgs.size()) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitFuncdef: Do not math previous function Declaration or Same name of Variable exists in line " << lineNum << " --" << fName << endl;
			exit(-1);
		}
		else {
			declFuncExist = symTabs[_GlobalFuncName_]->symbolExists(fName);
			symTabs[_GlobalFuncName_]->addSymbol(fName, {funcType, curFuncRetArgs, curFuncArgName});
		}

		if(!declFuncExist) {
			symTabs[fName] = new SymbolTable();
		}

		for(int i = 1; i < ctx->name().size(); ++i) {
			string name = ctx->name(i)->getText();
			if(!declFuncExist) symTabs[fName]->addSymbol(name, {varType});
			else {
				symTabs[fName]->changeSymbolName(to_string(i), name);
				changeDestStr(fName, to_string(i), name, i);
			}
		}

		scopeLevel.clear();
		scopeLevel.push_back(0);

		curFuncName = ctx->name(0)->getText();
		curScopeName = "";
		FuncReturned = false;
		visit(ctx->blockstmt());
		return nullptr;
    }

	any visitBlockstmt(BParser::BlockstmtContext *ctx) override {
		//cout << ctx->getText() << endl;
		if(curScopeName == "") curScopeName = curFuncName;
		else {
			(*scopeLevel.rbegin())++;
			curScopeName = fullScope();

			SymbolTable* scopeSymTab = new SymbolTable();
			symTabs[curScopeName] = scopeSymTab;

			scopeLevel.push_back(0);
		}

    	for (auto stmt : ctx->statement()) {
      	    visit(stmt);
    	}
		
		if(scopeLevel.size() == 1 && !FuncReturned) {
			int lineNum = ctx->getStart()->getLine();
			switch(int c = AssignExpr({_GlobalFuncName_, curFuncName}, make_pair(emptydt(tyVOID), false), -1, false)) {
			case -4:
			case -3:
			case -2:
			case -1:
				cerr << endl << "[ERROR] visitBlockstmt: Automatic Function add void return had unknown error in line " << lineNum << " --" << curFuncName << endl;
				exit(-1);
			}
		}

		scopeLevel.pop_back();
		if(scopeLevel.empty()) curFuncName = curScopeName = _GlobalFuncName_;
		else {
			curScopeName = fullScope(scopeLevel.size()-1);
		}

    	return nullptr;
    }

    /*any visitAutostmt(BParser::AutostmtContext *ctx) override {
    	// get current symbol table
		return nullptr;

		SymbolTable *stab = symTabs[curScopeName];

		// You can retrieve the variable names and constants using ctx->name(i) and ctx->constant(i)
		for (int i=0, j=0; i < ctx->name().size(); i++) {
			
			string varName = ctx->name(i)->getText();
			if(stab->symbolExists(varName)) {
				int lineNum = ctx->getStart()->getLine();
				cerr << endl << "[ERROR] visitAutostmt: Duplicate Variable Names in line " << lineNum << " --" << varName << endl;
				exit(-1); // error
			}
			enum Types varType = tyAUTO;				// default type

			// if initialized, get constant type
			int idx_assn = 1 + i*2 + j*2 + 1;  // auto name (= const)?, name (= const)?, ...
			if (ctx->children[idx_assn]->getText().compare("=") == 0) { 
				if (ctx->constant(j)) {  
					varType = any_cast<Types>( visit(ctx->constant(j)) );   // returns init constant type
					j++;
				}
			}

			stab->addSymbol(varName, {varType});
		}
    	return nullptr;
    }

    any visitConstant(BParser::ConstantContext *ctx) override {
		if (ctx->INT()) return tyINT;
		else if (ctx->REAL()) return tyDOUBLE;
		else if (ctx->STRING()) return tySTRING;
		else if (ctx->BOOL()) return tyBOOL;
		else if (ctx->CHAR()) return tyCHAR;

		cerr << "[ERROR] unrecognizable constant is used for initialization: " << ctx->children[0]->getText() << endl;
		exit(-1);
        return nullptr;
    }

	any visitDeclstmt(BParser::DeclstmtContext *ctx) override {
		return nullptr;
		SymbolAttributes funcat;
		funcat.type = tyFUNCTION;
		funcat.retArgTypes.push_back(tyAUTO);

        string functionName = ctx->name()->getText();
        
		// You can retrieve and visit the parameter type list
		for (int i=1; i < ctx->AUTO().size(); i++) {
			funcat.retArgTypes.push_back(tyAUTO);	
		}

		if(symTabs[_GlobalFuncName_]->symbolExists(functionName)) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitDeclStmt: Duplicate function Declaration or Same name of Variable exists in line " << lineNum << " --" << functionName << endl;
			exit(-1);
		}
		
		SymbolTable *gstab = symTabs[_GlobalFuncName_];
		SymbolTable *stab = symTabs[curScopeName];
		gstab->addSymbol(functionName, funcat);
		stab->addSymbol(functionName, funcat);
        return nullptr;
    }

	any visitFuncdef(BParser::FuncdefContext *ctx) override {
		curFuncName = ctx->name(0)->getText();
		curScopeName = "";
		scopeLevel.clear();
		scopeLevel.push_back(0);
		visit(ctx->blockstmt());
		return nullptr;
    }

	any visitBlockstmt(BParser::BlockstmtContext *ctx) override {
		if(curScopeName == "") curScopeName = curFuncName;
		else {
			(*scopeLevel.rbegin())++;
			curScopeName = fullScope();
			scopeLevel.push_back(0);
		}

    	for (auto stmt : ctx->statement()) {
      	    visit(stmt);
    	}

		scopeLevel.pop_back();
		if(scopeLevel.empty()) curFuncName = curScopeName = _GlobalFuncName_;
		else curScopeName = fullScope();
		
    	return nullptr;
    }
*/
    
	any visitDirective(BParser::DirectiveContext *ctx) override {
		string dir = ctx->SHARP_DIRECTIVE()->getText();
		vector<vector<string>> r = diranalysis(dir);
		//directiveanalysis(dir);

		if(r.empty()) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitDirective: Directive has Error in line " << lineNum << endl;
			exit(-1);
		}

		if(r[0][0] == "include") haveHeader = true;
		else if(r[0][0] == "define") {
			if(r.size() == 1) macroVar.insert(r[0][1]);
			else macroFunc[r[0][1]] = r[1].size();
		}

        return nullptr;
	}

	any visitStatement(BParser::StatementContext *ctx) override {
		visit(ctx->children[0]);
        return nullptr;
    }

	any visitIfstmt(BParser::IfstmtContext *ctx) override {
		pair<dt, bool> val = any_cast<pair<dt, bool>> (visit(ctx->expr()));
		int lineNum = ctx->getStart()->getLine();
		addPrereq(val.first, emptydt(tyBOOL));
		AssignExpr(val, lineNum, -1, true);

		if(ctx->statement(0)->blockstmt()) {
			visit(ctx->statement(0));
			//cout << "Block!" << endl;
		}
		else {
			hiddenScope = true;
			visit(ctx->statement(0));
			hiddenScope = false;
			//cout << "Not Block!" << endl;
		}
		
		//cout << ctx->statement(0)->getText() << endl;
		if (ctx->ELSE()) {
			if(ctx->statement(1)->blockstmt()) {
				visit(ctx->statement(1));
				//cout << "Block!" << endl;
			}
			else {
				hiddenScope = true;
				visit(ctx->statement(1));
				hiddenScope = false;
			}
		}
        return nullptr;
    }
	
    any visitWhilestmt(BParser::WhilestmtContext *ctx) override {
        pair<dt, bool> val = any_cast<pair<dt, bool>> (visit(ctx->expr()));
		int lineNum = ctx->getStart()->getLine();
		addPrereq(val.first, emptydt(tyBOOL));
		AssignExpr(val, lineNum, -1, true);

		if(ctx->statement()->blockstmt()) {
			visit(ctx->statement());
		}
		else {
			hiddenScope = true;
			visit(ctx->statement());
			hiddenScope = false;
		}
        return nullptr;
    }

    any visitExpressionstmt(BParser::ExpressionstmtContext *ctx) override {
		pair<dt, bool> val = any_cast<pair<dt, bool>> (visit(ctx->expression()));
		if(!ctx->expression()->ASSN()) {
			int lineNum = ctx->getStart()->getLine();
			AssignExpr(val, lineNum);
		}
        return nullptr;
    }

	any visitReturnstmt(BParser::ReturnstmtContext *ctx) override {
		SymbolAttributes funcAtt = symTabs[_GlobalFuncName_]->getSymbolAttributes(curFuncName);
		int lineNum = ctx->getStart()->getLine();
		pair<dt, bool> val = make_pair(emptydt(tyVOID), false);

		if (ctx->expression()) {
			val = any_cast<pair<dt, bool>> (visit(ctx->expression()));
		}

		int c;
		switch(c = AssignExpr({_GlobalFuncName_, curFuncName, 0}, val, lineNum, false)) {
		case -1:
			cerr << endl << "[ERROR] visitReturnstmt: cannot infer types of return value by auto variable in line " << lineNum << " --" << curFuncName << endl;
			exit(-1);
		case -2:
			cerr << endl << "[ERROR] visitReturnstmt: types does not match with return value in line " << lineNum << " --" << curFuncName << endl;
			exit(-1);
		default:
			break;
		}

		if(scopeLevel.size() == 1 && !hiddenScope) {
			//cout << "&&" << curFuncName << ' ' << scopeLevel[0] << endl;
			FuncReturned = true;
		}

        return nullptr;
    }

    any visitNullstmt(BParser::NullstmtContext *ctx) override {
        return nullptr;
    }

    any visitExpr(BParser::ExprContext *ctx) override {	
		pair<dt, bool> ret;
		// unary operator
        if(ctx->atom()) { //+, -, !
			pair<dt, bool> ret = any_cast<pair<dt, bool>>( visit(ctx->atom()) );
			if(getType(ret.first) == tyAUTO) {
				int lineNum = ctx->getStart()->getLine();
				string varName = ret.first.var;
				cerr << endl << "[ERROR] visitExpr: Cannot infer type by auto variable in line " << lineNum << " --" << varName << endl;
				exit(-1); // error
			}
			return ret;
        }
		// binary operator
		else if (ctx->MUL() || ctx->DIV() || ctx->PLUS() || ctx->MINUS() || 
		 		ctx->GT() || ctx->GTE() || ctx->LT() || ctx->LTE() || ctx->EQ() || ctx->NEQ() ||
		 		ctx->AND() || ctx->OR() ) {

			pair<dt, bool> a = any_cast<pair<dt, bool>>( visit(ctx->expr(0)) );
			pair<dt, bool> b = any_cast<pair<dt, bool>>( visit(ctx->expr(1)) );
			dt da = a.first;
			dt db = b.first;

			Types ta = getType(a.first);
			Types tb = getType(b.first);

			bool ua = a.second;
			bool ub = b.second;

			if(ta == tyAUTO || tb == tyAUTO) {
				string varName = ta == tyAUTO ? a.first.var : b.first.var;
				int lineNum = ctx->getStart()->getLine();
				cerr << endl << "[ERROR] visitExpr: Cannot infer type by auto variable in line " << lineNum << " --" << varName << endl;
				exit(-1); // error
			}
			else if(ta == tyANY && tb == tyANY) {
				ret = make_pair(emptydt(tyANY), false);
			}
			else if(ta == tyANY || tb == tyANY) {
				if(ta != tyANY) ret = a;
				else ret = b;
				ret.second = (ta == tyUNDEFINED || tb == tyUNDEFINED);
			}
			else if(ta != tb && ta != tyUNDEFINED && tb != tyUNDEFINED) {
				int lineNum = ctx->getStart()->getLine();
				cerr << endl << "[ERROR] visitExpr: Type does not match in line " << lineNum << " --" << mnemonicTypes[ta] << ", " << mnemonicTypes[tb] << endl;
				exit(-1); // error
			}
			else if(ta == tyUNDEFINED && tb == tyUNDEFINED) {
				addPrereq(da, db);
				ret = a;
				ret.second = true;
			}
			else if(ta == tyUNDEFINED) {
				addPrereq(da, emptydt(tb));
				ret = b;
				ret.second = true;
			}
			else if(tb == tyUNDEFINED) {
				addPrereq(db, emptydt(ta));
				ret = a;
				ret.second = true;
			}
			else {
				ret.first = emptydt(ta);
				ret.second = false;
			}

			if(ctx->MUL() || ctx->DIV() || ctx->PLUS() || ctx->MINUS()) {
				return ret;
			}
			else {
				return make_pair(emptydt(tyBOOL), ret.second);
			}
		}
		// ternary operator
		/* TODO: Check tyANY */
		else if (ctx->QUEST()) {
			pair<dt, bool> a = any_cast<pair<dt, bool>>( visit(ctx->expr(0)) );
			pair<dt, bool> b = any_cast<pair<dt, bool>>( visit(ctx->expr(1)) );
			pair<dt, bool> c = any_cast<pair<dt, bool>>( visit(ctx->expr(2)) );

			dt da = a.first;
			dt db = b.first;
			dt dc = c.first;

			Types ta = getType(a.first);
			Types tb = getType(b.first);
			Types tc = getType(c.first);

			bool ua = a.second;
			bool ub = b.second;
			bool uc = b.second;

			if(ta == tyAUTO || tb == tyAUTO || tc == tyAUTO) {
				string varName;
				if(ta == tyAUTO) varName = a.first.var;
				else if(tb == tyAUTO) varName = b.first.var;
				else varName = c.first.var;

				int lineNum = ctx->getStart()->getLine();
				cerr << endl << "[ERROR] visitExpr: Cannot infer type by auto variable in line " << lineNum << " --" << varName << endl;
				exit(-1); // error
			}
			
			
			if(ta = tyUNDEFINED) {
				addPrereq(da, emptydt(tyBOOL));
			}

			if(tb == tyANY && tc == tyANY) {
				ret = make_pair(emptydt(tyANY), false);
			}
			else if(tb == tyANY || tc == tyANY) {
				if(tb != tyANY) ret = b;
				else ret = c;
				ret.second = (tb == tyUNDEFINED || tc == tyUNDEFINED);
			}
			else if(tb != tc && tb != tyUNDEFINED && tc != tyUNDEFINED) {
				int lineNum = ctx->getStart()->getLine();
				cerr << endl << "[ERROR] visitExpr: Type does not match in line " << lineNum << " --" << mnemonicTypes[tb] << ", " << mnemonicTypes[tc] << endl;
				exit(-1); // error
			}
			else if(tb == tyUNDEFINED && tc == tyUNDEFINED) {
				addPrereq(db, dc, 'v');
				ret = b;
				ret.second = true;
			}
			else if(tb == tyUNDEFINED) {
				addPrereq(db, emptydt(tc), 'v');
				ret = c;
				ret.second = true;
			}
			else if(tc == tyUNDEFINED) {
				addPrereq(dc, emptydt(tb), 'v');
				ret = b;
				ret.second = true;
			}
			else {
				ret.first = emptydt(tb);
				ret.second = false;
			}

			return ret;
		}
		else {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitExpr: unrecognized ops in Line " << lineNum << " --" << ctx->children[1]->getText() << endl;
			exit(-1); // error
        }	
        return nullptr;
    }
   
    any visitAtom(BParser::AtomContext *ctx) override {
		if (ctx->expression()) { // ( expression )
			return any_cast<pair<dt, bool>>( visit(ctx->expression()) );
		}
		else	// name | constant | funcinvocation
			return any_cast<pair<dt, bool>>( visit(ctx->children[0]) );
			
        return nullptr;
    }
    
    any visitExpression(BParser::ExpressionContext *ctx) override {
		pair<dt, bool> ret = any_cast<pair<dt, bool>>( visit(ctx->expr()) );

		bool ur = ret.second;
		Types retType = getType(ret.first);

        if (ctx->ASSN()) { // assignment
			int lineNum = ctx->getStart()->getLine();
			dt ass = any_cast<pair<dt, bool>>( visit(ctx->name()) ).first;

			switch(AssignExpr(ass, ret, lineNum)) {
			case -1:
				cerr << endl << "[ERROR] visitExpression: cannot infer types of value by auto variable in line " << lineNum << " --" << ret.first.var << endl;
				exit(-1);
			case -2:
				cerr << endl << "[ERROR] visitExpression: types does not match while assigning in line " << lineNum << " --" << mnemonicTypes[getType(ass)] << ", " << mnemonicTypes[getType(ret.first)] << endl;
				exit(-1);
			case -3:
				cerr << endl << "[ERROR] visitExpression: Macro Function assigned in auto variable in line " << lineNum << " --" << ass.var << endl;
				exit(-1);
			case -4:
				cerr << endl << "[ERROR] visitExpression: Macro Function cannot assigned in line " << lineNum << endl;
				exit(-1);
			default:
				return ret;
			}
		}
		else {
			/* TODO : Not Assign Expr (??)*/
		}

        return ret;
    }

    any visitFuncinvocation(BParser::FuncinvocationContext *ctx) override {
		if(macroFuncInvocation(ctx)) {
			return make_pair(emptydt(tyANY), false);
		}

		string funcName = ctx->name()->getText();
		if(findSymbol(funcName) != _GlobalFuncName_ && findSymbol(funcName) != "") {
			int lineNum = ctx->getStart()->getLine();
			cerr << "[Error] visitFuncinvocation: instead of function, same name of variable exists in line " << lineNum << " --" << funcName << endl;
			exit(-1);
		}
		if(!symTabs[_GlobalFuncName_]->symbolExists(funcName)) {
			if(headerFuncInvocation(ctx)) {
				return make_pair(emptydt(tyANY), false);
			}
			int lineNum = ctx->getStart()->getLine();
			cerr << "[Error] visitFuncinvocation: function does not exist in line " << lineNum << " --" << funcName << endl;
			exit(-1);
		}

		SymbolAttributes funcAtt = symTabs[_GlobalFuncName_]->getSymbolAttributes(funcName);
		if(ctx->expr().size() + 1 != funcAtt.retArgTypes.size()) {
			int lineNum = ctx->getStart()->getLine();
			cerr << "[Error] visitFuncinvocation: number of function arguments does not match in line " << lineNum << " --" << funcName << endl;
			exit(-1);
		}

		//cout << ctx->name()->getText() << "(";
		for (int i=0; i < ctx->expr().size(); i++) {
			dt ass = {funcName, funcAtt.argName[i+1], i+1};
			pair<dt, bool> val = any_cast<pair<dt, bool>>(visit(ctx->expr(i)));
			if(getType(val.first) == tyANY) continue;
			int lineNum = ctx->getStart()->getLine();

			int c;
			switch(c = AssignExpr(ass, val, lineNum)) {
			case -1:
				cerr << endl << "[ERROR] visitFunctioninvocation: cannot infer types of Function Parameters by auto variable in line " << lineNum << " --" << funcName << " - " << funcAtt.argName[i+1] << endl;
				exit(-1);
			case -2:
				cerr << endl << "[ERROR] visitFunctioninvocation:: types does not match with Function Parameters in line " << lineNum << " --" << funcName << " - " << funcAtt.argName[i+1] << endl;
				exit(-1);
			}
		}

		dt ret;
		ret.scope = _GlobalFuncName_;
		ret.var = funcName;
        return make_pair(ret, funcAtt.retArgTypes[0] == tyUNDEFINED);
    }
    
    any visitName(BParser::NameContext *ctx) override {
		dt ret;
		ret.var = ctx->getText();
		
		if((ret.scope = findSymbol(ret.var)) == "") {
			if(macroVar.count(ret.var) > 0) {
				return make_pair(emptydt(tyANY), false);
			}
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitName: variable does not exist in line " << lineNum << " --" << ret.var << endl;
			exit(-1);
		}

		Types t = symTabs[ret.scope]->getSymbolAttributes(ret.var).type;
		if(t == tyFUNCTION) {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitName: Function used without parenthesis in line " << lineNum << " --" << ret.var << endl;
			exit(-1);
		}
		
		return make_pair(ret, t == tyUNDEFINED);
    }
};

/*
 * STEP 3. print code
 */


class PrintTreeVisitor : public BBaseVisitor {
private:
	vector<int> scopeLevel;
    string curFuncName;
    string curScopeName;

	string fullScope(int n = -2) {
		if(n == -2) n = scopeLevel.size();
		else if(n == -1) return _GlobalFuncName_;
		else if(n == 0) return curFuncName;

		string ret = curFuncName + "_$";
		for(int i = 0; i < n && i < scopeLevel.size(); ++i) {
			ret += to_string(scopeLevel[i]) + "_";
		}
		ret.pop_back();
		return ret;
	}

public:
    any visitProgram(BParser::ProgramContext *ctx) override {
    	// Perform some actions when visiting the program
		scopeLevel.clear();
		scopeLevel.push_back(0);
		curScopeName = curFuncName = _GlobalFuncName_;

    	for (int i=0; i< ctx->children.size(); i++) {
      	    visit(ctx->children[i]);
    	}
    	return nullptr;
    }
    
    any visitDirective(BParser::DirectiveContext *ctx) override {
		cout << ctx->SHARP_DIRECTIVE()->getText();
		cout << endl;
        return nullptr;
    }

    any visitDefinition(BParser::DefinitionContext *ctx) override {
		visit(ctx->children[0]);
        return nullptr;
    }

    any visitFuncdef(BParser::FuncdefContext *ctx) override {
		// Handle function definition
        string functionName = ctx->name(0)->getText();
		SymbolAttributes sa = symTabs[_GlobalFuncName_]->getSymbolAttributes(functionName);

		cout << mnemonicTypes[sa.retArgTypes[0]] << " " << functionName << "(" ;
        // You can retrieve and visit the parameter list using ctx->name(i)
		for (int i=1; i < ctx->name().size(); i++) {
			if (i != 1) cout << ", ";
			cout << mnemonicTypes[sa.retArgTypes[i]] << " " << ctx->name(i)->getText();		
		}
		cout << ")";

		// visit blockstmt

		curFuncName = ctx->name(0)->getText();
		curScopeName = "";
		scopeLevel.clear();
		scopeLevel.push_back(0);
		visit(ctx->blockstmt());
        return nullptr;
    }

    any visitStatement(BParser::StatementContext *ctx) override {
		visit(ctx->children[0]);
        return nullptr;
    }

    any visitAutostmt(BParser::AutostmtContext *ctx) override {
    	// You can retrieve the variable names and constants using ctx->name(i) and ctx->constant(i)
		//cout << "auto "
		for (int i=0, j=0; i < ctx->name().size(); i++) {
			//if (i != 0) cout << " ,";
			string varName = ctx->name(i)->getText();

			cout << mnemonicTypes[symTabs[curScopeName]->getSymbolAttributes(varName).type] << " " << varName;


			int idx_assn = 1 + i*2 + j*2 + 1;  // auto name (= const)?, name (= const)?, ...
			if (ctx->children[idx_assn]->getText().compare("=") == 0) { 
				if (ctx->constant(j)) {
					cout << " = ";    
					visit(ctx->constant(j));
					j++;
				}
			}
			cout << ";" << endl;
		}
    	return nullptr;
    }

    any visitDeclstmt(BParser::DeclstmtContext *ctx) override {
		// Handle function declaration
        string functionName = ctx->name()->getText();
		SymbolAttributes sa = symTabs[_GlobalFuncName_]->getSymbolAttributes(functionName);
		cout << mnemonicTypes[sa.retArgTypes[0]] << " " << functionName << "(" ;
        
		// You can retrieve and visit the parameter type list
		for (int i=1; i < ctx->AUTO().size(); i++) {
			if (i != 1) cout << ", ";
			cout << mnemonicTypes[sa.retArgTypes[i]] << ' ';		
		}
		cout << ");" << endl;
        return nullptr;
    }

	any visitBlockstmt(BParser::BlockstmtContext *ctx) override {
		cout << "{" << endl;
		if(curScopeName == "") curScopeName = curFuncName;
		else {
			(*scopeLevel.rbegin())++;
			
			curScopeName = fullScope();
			scopeLevel.push_back(0);
		}

    	for (auto stmt : ctx->statement()) {
      	    visit(stmt);
    	}

		scopeLevel.pop_back();
		if(scopeLevel.empty()) curFuncName = curScopeName = _GlobalFuncName_;
		else {
			curScopeName = fullScope(scopeLevel.size()-1);
		}

		cout << "}" << endl;
    	return nullptr;
    }

    any visitIfstmt(BParser::IfstmtContext *ctx) override {
		cout << "if (";
		visit(ctx->expr());
		cout << ") " ;

		visit(ctx->statement(0));
		if (ctx->ELSE()) {
	   		cout << endl << "else ";
	   		visit(ctx->statement(1));
		}
        return nullptr;
    }

    any visitWhilestmt(BParser::WhilestmtContext *ctx) override {
        cout << "while (";
        visit(ctx->expr());
        cout << ") ";
        visit(ctx->statement());
        return nullptr;
    }

    any visitExpressionstmt(BParser::ExpressionstmtContext *ctx) override {
		visit(ctx->expression());
		cout << ";" << endl;
        return nullptr;
    }

    any visitReturnstmt(BParser::ReturnstmtContext *ctx) override {
		cout << "return";
		if (ctx->expression()) {
			cout << " (";
			visit(ctx->expression());
			cout << ")";
		}
		cout << ";" << endl;
        return nullptr;
    }

    any visitNullstmt(BParser::NullstmtContext *ctx) override {
		cout << ";" << endl;
        return nullptr;
    }

    any visitExpr(BParser::ExprContext *ctx) override {
		// unary operator
        if(ctx->atom()) {
            if (ctx->PLUS()) cout << "+";
            else if (ctx->MINUS()) cout << "-";
	    	else if (ctx->NOT()) cout << "!";
	    	visit(ctx->atom()); 
        }
		// binary operator
		else if (ctx->MUL() || ctx->DIV() || ctx->PLUS() || ctx->MINUS() || 
		 		ctx->GT() || ctx->GTE() || ctx->LT() || ctx->LTE() || ctx->EQ() || ctx->NEQ() ||
		 		ctx->AND() || ctx->OR() ) {
	    	visit(ctx->expr(0));
	    	cout << " " << ctx->children[1]->getText() << " "; // print binary operator
	    	visit(ctx->expr(1));
		}
		// ternary operator
		else if (ctx->QUEST()) {
			visit(ctx->expr(0));
			cout << " ? ";
			visit(ctx->expr(1));
			cout << " : ";
			visit(ctx->expr(2));
		}
		else {
			int lineNum = ctx->getStart()->getLine();
			cerr << endl << "[ERROR] visitExpr: unrecognized ops in line " << lineNum << " --" << ctx->children[1]->getText() << endl;
			exit(-1); // error
        }	
        return nullptr;
    }
   
    any visitAtom(BParser::AtomContext *ctx) override {
		if (ctx->expression()) { // ( expression )
			cout << "(";
			visit(ctx->expression());
			cout << ")";
		}
		else	// name | constant | funcinvocation
			visit(ctx->children[0]);
        return nullptr;
    }
    
    any visitExpression(BParser::ExpressionContext *ctx) override {
        if (ctx->ASSN()) { // assignment
	   		visit(ctx->name());
	  		 cout << " = ";
		}
		visit(ctx->expr());
        return nullptr;
    }

    any visitFuncinvocation(BParser::FuncinvocationContext *ctx) override {
		cout << ctx->name()->getText() << "(";
		for (int i=0; i < ctx->expr().size(); i++) {
			if (i != 0) cout << ", ";
			visit(ctx->expr(i));
		}
		cout << ")";
        return nullptr;
    }
    
    any visitConstant(BParser::ConstantContext *ctx) override {
        cout << ctx->children[0]->getText();
        return nullptr;
    }
    
    any visitName(BParser::NameContext *ctx) override {
		cout << ctx->getText() << " ";
        return nullptr;
    }
};

int main(int argc, const char* argv[]) {
    if (argc < 2) {
        cerr << "[Usage] " << argv[0] << "  <input-file>\n";
        exit(0);
    }
    std::ifstream stream;
    stream.open(argv[1]);
    if (stream.fail()) {
        cerr << argv[1] << " : file open fail\n";
        exit(0);
    }

	//cout << "/*-- B2C ANTLR visitor --*/\n";

	ANTLRInputStream inputStream(stream);
	BLexer lexer(&inputStream);
	CommonTokenStream tokenStream(&lexer);
	BParser parser(&tokenStream);
	ParseTree* tree = parser.program();

	// STEP 1. visit parse tree and build symbol tables for functions (PA#1)
	cout << endl << "/*** STEP 1. BUILD SYM_TABS *************" << endl;
	SymbolTableVisitor SymtabTree;
	SymtabTree.visit(tree);
	cout <<         " ---    end of step 1       ------------*/" << endl;

	clearSymTabs();

	// STEP 2. visit parse tree and perform type inference for 'auto' typed variables and functions (PA#2)
	cout << endl << "/*** STEP 2. ANALYZE TYPES  *************" << endl;
	TypeAnalysisVisitor AnalyzeTree;
	AnalyzeTree.visit(tree);
	cout <<         " ---    end of step 2       ------------*/" << endl;

	// STEP 3. visit parse tree and print out C code with correct types
	cout << endl << "/*** STEP 3. TRANSFORM to C *************/" << endl;
	PrintTreeVisitor PrintTree;
	PrintTree.visit(tree);

	clearSymTabs();

	return 0;
}
