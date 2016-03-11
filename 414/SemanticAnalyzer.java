import java.util.List;
import java.util.Vector;

public class SemanticAnalyzer implements ASTVisitor {
	private VariableEnvironment varEnv;
	private FunctionEnvironment funcEnv;
	private TypeEnvironment typeEnv;
	private VariableEnvironment ret$;
	private AATBuildTree bt;
	private int wordSize;
	private int funcLength;
	private int funcOffset;
	private Label globalEnd;

	public SemanticAnalyzer() {

		varEnv = new VariableEnvironment();
		funcEnv = new FunctionEnvironment();
		funcEnv.addBuiltinFunctions();
		typeEnv = new TypeEnvironment();
		bt = new AATBuildTree();
		ret$ = new VariableEnvironment();
		wordSize = MachineDependent.WORDSIZE;
		globalEnd = new Label();

	}	
	
	// DONE
	@Override
	public Object VisitArrayVariable(ASTArrayVariable arrayVar) {
		/*
		 * Parameters: arrayVar
		 * 
		 * Types within the Parameters: ASTVariable base, ASTExpression index,
		 * int line
		 * 
		 * Steps: Accept the base Accept the index Check that the index is of
		 * type integer If not Print error Check that the base is of array type
		 * If it is Cast the base array as an array type If not Print an error
		 */
		TypeClass baseClass, indexClass;

		baseClass = (TypeClass) arrayVar.base().Accept(this);
		indexClass = (TypeClass) arrayVar.index().Accept(this);

		Type baseType = baseClass.type();
		Type indexType = indexClass.type();

		if (indexType != IntegerType.instance()) {
			CompError.message(arrayVar.line(), "This " + indexType
					+ " is not of type integer.");
		}

		if (baseType instanceof ArrayType) {
			Type arrayType = ((ArrayType) baseType).type();
			System.out.println("SSSSSSSSS");
			return new TypeClass(arrayType, bt.arrayVariable(baseClass.value(),
					indexClass.value(), wordSize));
		} else {
			 
			CompError.message(arrayVar.line(), "This variable " + baseType
					+ " is not of array type.");
		}
		return new TypeClass(IntegerType.instance(), null);
	}

	// DONE
	@Override
	public Object VisitAssignmentStatement(ASTAssignmentStatement assignStmt) {
		/*
		 * Parameters: assignStmt
		 * 
		 * Types within the Parameters: ASTVariable variable, ASTExpression
		 * value, int line
		 * 
		 * Steps: Accept the variable (left side) Accept the value (right side)
		 * Check that the left and right sides are of the same type If not Print
		 * error Else Return null
		 */

		TypeClass lhsClass, rhsClass;
		lhsClass = (TypeClass) assignStmt.variable().Accept(this);
		rhsClass = (TypeClass) assignStmt.value().Accept(this);

		Type lhs, rhs;
		lhs = lhsClass.type();
		rhs = rhsClass.type();

		if (lhs != rhs) {
			CompError
					.message(assignStmt.line(), "The type of the left side "
							+ lhs
							+ " does not match the type of the right side "
							+ rhs);
		}

		return bt.assignmentStatement(lhsClass.value(), rhsClass.value());
	}

	// DONE
	@Override
	public Object VisitBaseVariable(ASTBaseVariable base) {
		/*
		 * Parameters: base
		 * 
		 * Types within the Parameters: String name, int line
		 * 
		 * Steps: Accept the base Check that the base is within the variable
		 * environment If not Print an error and return integer type Else Return
		 * the type of the base
		 */
		VariableEntry baseEntry = varEnv.find(base.name());
		if (baseEntry == null) {
			CompError.message(base.line(), "This variable " + base.name()
					+ " was never defined within this scope");
			return new TypeClass(IntegerType.instance(), null);
		} else {
			return new TypeClass(baseEntry.type(), bt.baseVariable(baseEntry
					.offset()));
		}
	}

	// DONE
	@Override
	public Object VisitBooleanLiteral(ASTBooleanLiteral boolLiteral) {
		/*
		 * Parameters: boolLiteral
		 * 
		 * Types within the Parameters: boolean value, int line
		 * 
		 * Steps: Return a boolean type
		 */
		int returnValue = boolLiteral.value() ? 1 : 0;

		return new TypeClass(BooleanType.instance(),
				bt.constantExpression(returnValue));

	}

	// DONE
	@Override
	public Object VisitClass(ASTClass class_s) {
		/*
		 * Parameters: class_s
		 * 
		 * Types within the Parameters: String name, ASTInstanceVariableDefs
		 * variabledefs, int line
		 * 
		 * Steps: Accept each variable definition with in the list of variable
		 * definitions Create a new ClassType with the entry as its type Insert
		 * this class type into the type environment Return null
		 */
		VariableEnvironment classEnv = (VariableEnvironment) class_s
				.variabledefs().Accept(this);
		ClassType classType = new ClassType(classEnv);
		typeEnv.insert(class_s.name(), classType);

		return new TypeClass(IntegerType.instance(), (AATExpression)null);
	}

	// DONE
	@Override
	public Object VisitClasses(ASTClasses classes) {
		/*
		 * Parameters: classes
		 * 
		 * Types within the Parameters: ASTClass classs
		 * 
		 * Steps: Loop through each of class within the list of classes Accept
		 * each class Return null
		 */
		for (int i = 0; i < classes.size(); i++) {
			classes.elementAt(i).Accept(this);
		}
		//same as above
		return new TypeClass(null, null);
	}

	// DONE
	@Override
	public Object VisitClassVariable(ASTClassVariable classVar) {
		/*
		 * Parameters: classVar
		 * 
		 * Types within the Parameters: ASTVariable base, String variable, int
		 * line
		 * 
		 * Steps: Accept the base Accept the variables from the class Check that
		 * the base is of a class type if it is Cast the base as a Class Type
		 * Create a new entry for the variable environment Try to find that
		 * entry within the variable environment If it is found then return the
		 * type of that entry Else print out an error message Else Print out an
		 * error message Return Integer Type
		 */
		TypeClass baseRecordClass = (TypeClass) classVar.base().Accept(this);
		Type baseRecord = baseRecordClass.type();
		String instanceVar = classVar.variable();

		if (baseRecord instanceof ClassType) {
			ClassType classType = (ClassType) baseRecord;
			VariableEnvironment env = classType.variables();
			VariableEntry entry = env.find(instanceVar);
			if (entry != null) {
				return new TypeClass(entry.type(), bt.classVariable(
						baseRecordClass.value(), entry.offset()));
			}

			CompError.message(classVar.line(), "The variable " + instanceVar
					+ " was never created within this class " + classType);
		} else {
			CompError.message(classVar.line(), "This instance variable "
					+ instanceVar + " was never defined.");
		}
		return new TypeClass(IntegerType.instance(), (AATExpression) null);
	}

	// DONE
	@Override
	public Object VisitDoWhileStatement(ASTDoWhileStatement doWhile) {
		/*
		 * Parameters: doWhile
		 * 
		 * Types within the Parameters: ASTExpression test, ASTStatement body,
		 * int line
		 * 
		 * Steps: Begin a variable scope Accept the do test Accept the body
		 * Check that the do test is of boolean type If not Print error Else End
		 * the variable scope Return null
		 */
		varEnv.beginScope();
		TypeClass test = (TypeClass) doWhile.test().Accept(this);

		AATStatement body = (AATStatement) doWhile.body().Accept(this);

		if (test.type() != BooleanType.instance()) {
			CompError.message(doWhile.line(), " The test " + test.type()
					+ " is not of a boolean type.");
		}

		varEnv.endScope();
		return bt.dowhileStatement(test.value(), body);
	}

	// DONE
	@Override
	public Object VisitEmptyStatement(ASTEmptyStatement empty) {
		/*
		 * Parameters: empty
		 * 
		 * Types within the Parameters: int line
		 * 
		 * Steps: Return null
		 */
		return bt.emptyStatement();
	}

	// DONE
	@Override
	public Object VisitForStatement(ASTForStatement forStmt) {
		/*
		 * Parameters: forStmt
		 * 
		 * Types within the Parameters: ASTStatement initialize, ASTExpression
		 * test, ASTStatement increment, ASTStatement body, int line
		 * 
		 * Steps: Accept the initialization statement Accept the test Accept the
		 * increment statement Check that the test statement evaluates to
		 * boolean type If not Print error Else Accept the body Return null
		 */
		AATStatement forInitialize = (AATStatement) forStmt.initialize()
				.Accept(this);
		TypeClass forTest = (TypeClass) forStmt.test().Accept(this);
		AATStatement forIncrement = (AATStatement) forStmt.increment().Accept(
				this);
		AATStatement forBody = (AATStatement) forStmt.body().Accept(this);

		if (forTest.type() != BooleanType.instance()) {
			CompError.message(forStmt.line(), " This for loop test " + forTest
					+ " does not have a boolean value when it is evaluated.");
		}
		return bt.forStatement(forInitialize, forTest.value(), forIncrement,
				forBody);
	}

	// DONE
	@Override
	public Object VisitFormal(ASTFormal formal) {
		/*
		 * Parameters: formal
		 * 
		 * Types within the Parameters: String type, String name, int line OR
		 * String type, String name, int arraydimension, int line
		 * 
		 * Steps: Check to see if the formal is within the variable environment
		 * If not Insert it into the variable environment Else Check to see if
		 * the formal type is within the type environment If not Print an error
		 * Else Return the formal type Check that the dimension of the formal is
		 * less that zero If it is Print an error Else Return formal type
		 */
		VariableEntry formalEntry = varEnv.find(formal.name());
		if (formalEntry == null) {
			varEnv.insert(formal.name(), formalEntry);
		}
		Type type_entry = typeEnv.find(formal.type());
		if (type_entry == null) {
			CompError.message(formal.line(), "The type " + formal.type()
					+ " is an invalid type.");
			type_entry = IntegerType.instance();
		}
		if (formal.arraydimension() < 0) {
			CompError.message(
					formal.line(),
					"There can be no negative array dimensions like "
							+ formal.type() + ".");
		}
		return type_entry;
	}

	// DONE
	@Override
	public Object VisitFormals(ASTFormals formals) {
		/*
		 * Parameters: formals
		 * 
		 * Types within the Parameters: ASTFormal formals
		 * 
		 * Steps: Create a Vector to put each formal into Loop through all of
		 * the formal parameters Accept each formal Return the Vector with
		 * formals in it
		 */
		Vector<Type> formalVector = new Vector<>();
		for (int i = 0; i < formals.size(); i++) {
			formalVector.addElement((Type) formals.elementAt(i).Accept(this));
		}
		return formalVector;
	}

	// TODO: DONE????
	@Override
	public Object VisitFunction(ASTFunction function) {
		/*
		 * Parameters: function
		 * 
		 * Types within the Parameters: String type, String name, ASTFormals
		 * formals, ASTStatement body, int line
		 * 
		 * Steps: Create a string for the name of the function Create a string
		 * for the function return type Create a list of all the formals within
		 * the function Create an entry for the function name Try to find the
		 * type of the function within the type environment If it isn't there
		 * Check if the function type is empty If not Create a vector for all of
		 * the formal arguments to go into Create an entry for the function
		 * environment Insert that entry and the formals into the function
		 * environment Otherwise Get all of the formals from the function into a
		 * list If the size of the function formals and the entry formals aren't
		 * the same Print an error Else Go through the list of the function
		 * formals and the list of formal types If they are not of the same type
		 * Print an error Check the return type of the function against the
		 * entry If they are not the same Print an error Otherwise Begin a new
		 * variable environment scope Loop through the list of formals Create an
		 * entry for each of the types of the formals Insert each entry into the
		 * variable environment Create a return entry with the result type
		 * Insert that return entry into the return environment End the variable
		 * environment scope Return null
		 */
		
		String functionName = function.name();
		String returnTypeName = function.type();
		ASTFormals formals = function.formals();
		List<Type> formalsTypes = (List<Type>) formals.Accept(this);
		Label start = new Label(functionName);
		Label end = new Label(functionName + "_");

		FunctionEntry functionEntry = funcEnv.find(functionName);
		Type result = typeEnv.find(function.type());

		if (functionEntry == null) {
			if (result != null) {
				Vector formalTypes = (Vector) formals.Accept(this);
				functionEntry = new FunctionEntry(result, formalTypes, start, end);
				funcEnv.insert(functionName, functionEntry);
			}
		} else {
			List<Type> expectedFormalsType = functionEntry.formals();
			if (formalsTypes.size() != expectedFormalsType.size()) {
				CompError
						.message(
								function.line(),
								"The number of parameters in "
										+ functionName
										+ " are different from the prototype within the function environment.");
			}

			for (int i = 0; i < expectedFormalsType.size()
					&& i < formalsTypes.size(); i++) {
				if (formalsTypes.get(i) != expectedFormalsType.get(i)) {
					CompError.message(function.line(),
							" The type of parameters in " + formalsTypes.get(i)
									+ " is different from the prototype "
									+ expectedFormalsType.get(i));
				}
			}
			Type expectedReturnType = functionEntry.result();
			if (result != expectedReturnType) {
				CompError
						.message(
								function.line(),
								"The return type in of the function "
										+ result
										+ " is different from the return type of the function "
										+ expectedReturnType);
			}
		}
		varEnv.beginScope();
		funcOffset = 0;
		int offsetParams = -4;
		for (int i = 0; i < formals.size(); i++) {
			ASTFormal formal = formals.elementAt(i);
			VariableEntry variableEntry = new VariableEntry(formalsTypes.get(i), offsetParams);
			offsetParams -= 4;
			varEnv.insert(formal.name(), variableEntry);
		}
		VariableEntry returnEnt = new VariableEntry(result);
		ret$.insert("return", returnEnt);
		globalEnd = functionEntry.endlabel();
		AATStatement funcBody = (AATStatement) function.body().Accept(this);
		varEnv.endScope();
		return bt.functionDefinition(funcBody, funcOffset, functionEntry.startlabel(),
				functionEntry.endlabel());
	}

	// TODO:  DONE????
	@Override
	public Object VisitFunctionCallExpression(
			ASTFunctionCallExpression functionCall) {
		/*
		 * Parameters: functionCall
		 * 
		 * Types within the Parameters: String name, int line OR String name,
		 * ASTExpression formal, int line
		 * 
		 * Steps: Check to see if the name of the function call is within the
		 * function environment If not Print an error Else Get the return type
		 * of the entry Create a list of all the formals within the function
		 * call Check that the size of the formals of the function call is the
		 * same as the size of formals of the entry If not Print an error Loop
		 * through the size of the formals of the function call and the formals
		 * of the entry Accept the types of each of them If the types don't
		 * match Print an error Return the return type Return an Integer Type
		 */
		Vector<AATExpression> typeC = new Vector<AATExpression> ();
		FunctionEntry functionEntry = funcEnv.find(functionCall.name());
		if (functionEntry == null) {
			CompError.message(functionCall.line(), "The function call "
					+ functionCall.name() + " was never declared.");
		} else {
			Type returnType = functionEntry.result();
			List<Type> expectedTypes = functionEntry.formals();
			if (functionCall.size() != expectedTypes.size()) {
				CompError.message(functionCall.line(),
						"The number of parameters of " + functionCall.name()
								+ " doesn't match the " + functionEntry);
			}
			for (int i = 0; i < expectedTypes.size() && i < functionCall.size(); i++) {
				//created typeclass here to add to the call expression
				TypeClass argType = (TypeClass) functionCall.elementAt(i).Accept(this);
				if (argType.type() != expectedTypes.get(i)) {
					CompError
							.message(
									functionCall.line(),
									"The types of the formals of the "
											+ functionCall.name()
											+ " are not the same as the formals of the "
											+ functionEntry);
				}
				typeC.addElement(argType.value());
				//create the vector actual arguments here after we have gotten each of the types checked
			}
			//create a new call expression to take actual arguments  and start label
			//create the new call expression which is the tree
			//return typeclass(treee, rettype)
			return new TypeClass(returnType, bt.callExpression(typeC, functionEntry.startlabel()));

		}
		return IntegerType.instance();
	}

	// TODO: DONE????
	@Override
	public Object VisitFunctionCallStatement(
			ASTFunctionCallStatement functionCall) {
		/*
		 * Parameters: functionCall
		 * 
		 * Types within the Parameters: String name, int line OR String name,
		 * ASTExpression formal, int line
		 * 
		 * Steps: Check to see if the function call statement is in the function
		 * environment If not Print an error Check to see that the functionCall
		 * parameter size is greater than zero Loop through the function call
		 * parameters Accept the type of each of the parameters Return null
		 */
		Vector<AATExpression> typeClass = new Vector<AATExpression> ();
		FunctionEntry entry = funcEnv.find(functionCall.name());
		if (entry == null) {
			CompError.message(functionCall.line(), "This function call "
					+ functionCall.name() + " was never declared");
		}
		if (functionCall.size() > 0) {
			for (int i = 0; i < functionCall.size(); i++) {
				TypeClass funcCallStat = (TypeClass) functionCall.elementAt(i).Accept(
						this);
				typeClass.addElement(funcCallStat.value()); 
			}
		}
		//return a call statement  tree of list above 
		return bt.callStatement(typeClass, entry.startlabel());
	}
	
	//TODO: DONE????
	@Override
	public Object VisitFunctionDefinitions(
			ASTFunctionDefinitions functionDefinitions) {
		/*
		 * Parameters: functionDefinitions
		 * 
		 * Types within the Parameters: ASTFunctionDefinition functiondefinition
		 * 
		 * Steps: Loop through all of the function definitions with in the
		 * program Accept each of them Return null
		 */
		
		AATStatement statement, statement2 = bt.emptyStatement();
//		if(functionDefinitions.size() == 1){
//			//check if there is 1 then do one statement in the seq
//			//what is the value of the TypeClass?
//			//what should the second one be?
//			statement = (AATStatement)functionDefinitions.elementAt(0).Accept(this);
//			//just single statement
//			return statement;
//		}
//		if (functionDefinitions.size() > 1){
			statement2 = (AATStatement) functionDefinitions.elementAt(0).Accept(this);
			for (int i = 1; i < functionDefinitions.size(); i++) {
				//if there are more than 1 do a seq for each of them
				//return the giant tree of all of all seq statements 
				statement = (AATStatement) functionDefinitions.elementAt(i).Accept(this);
				statement2 = bt.sequentialStatement(statement2, statement);
			}
//		}
		return statement2;
	}

	// DONE
	@Override
	public Object VisitIfStatement(ASTIfStatement ifStmt) {
		/*
		 * Parameters: ifStmt
		 * 
		 * Types within the Parameters: ASTExpression test, ASTStatement
		 * thenstatement, ASTStatement elsestatement, int line
		 * 
		 * Steps: Accept the type of the if statement Check that the if
		 * statement evaluates to a boolean type If not Print an error Accept
		 * the then statement Check to see if there is an else statement If not
		 * Accept the else statement Return null
		 */
		TypeClass test = (TypeClass) ifStmt.test().Accept(this);
		if (test.type() != BooleanType.instance()) {
			CompError.message(ifStmt.line(), "The test " + ifStmt
					+ " does not evaluate to a boolean type.");
		}

		AATStatement bodyStatement = (AATStatement) ifStmt.thenstatement()
				.Accept(this);
		AATStatement elseStatement = null;

		if (ifStmt.elsestatement() != null) {
		    elseStatement = (AATStatement)ifStmt.elsestatement().Accept(this);
		    return bt.ifStatement(test.value(), bodyStatement, elseStatement);
		}
		return bt.ifStatement(test.value(), bodyStatement, elseStatement);
	}

	// DONE
	@Override
	public Object VisitIntegerLiteral(ASTIntegerLiteral literal) {
		/*
		 * Parameters: literal
		 * 
		 * Types within the Parameters: int value, int line
		 * 
		 * Steps: Return an integer type
		 */
		AATExpression constantExp = bt.constantExpression(literal.value());
		TypeClass returnClass = new TypeClass(IntegerType.instance(),
				constantExp);

		return returnClass;
	}

	@Override
	public Object VisitInstanceVariableDef(ASTInstanceVariableDef variableDef) {
		/*
		 * Not Used
		 */
		return null;
	}

	// DONE
	@Override
	public Object VisitInstanceVariableDefs(ASTInstanceVariableDefs variableDefs) {
		/*
		 * Parameters: variableDefs
		 * 
		 * Types within the Parameters: ASTInstanceVariableDef variabledef
		 * 
		 * Steps: Create a local variable environment Loop through all of the
		 * variable definitions Accept each definition Check the type of each
		 * definition If the type is not found Print an error Else Create a new
		 * variable entry Insert it into the local variable environment Return
		 * the local variable environment
		 */
		int offset = 0;
		VariableEnvironment local = new VariableEnvironment();
		for (int i = 0; i < variableDefs.size(); i++) {
			ASTInstanceVariableDef def = variableDefs.elementAt(i);
			Type instanceType = GetType(def.type(), def.arraydimension(),
					variableDefs.elementAt(i).line(), variableDefs.elementAt(i)
							.name());
			if (instanceType == null) {
				CompError.message(variableDefs.elementAt(i).line(),
						"This type " + variableDefs.elementAt(i)
								+ "has not been defined");
			} else {
				VariableEntry classVarDef = new VariableEntry(instanceType, offset);
				local.insert(def.name(), classVarDef);
			}
			offset+=4;
		}
		return local;
	}

	// DONE
	@Override
	public Object VisitNewArrayExpression(ASTNewArrayExpression newArray) {
		/*
		 * Parameters: newArray
		 * 
		 * Types within the Parameters: String type, ASTExpression elements, int
		 * arraydimension, int line OR String type, ASTExpression elements, int
		 * line
		 * 
		 * Steps: Accept the elements within the array Check the type of the
		 * array If the type of the array is not of array type Print an error If
		 * the array dimension is a negative number Print an error Create a new
		 * array type element Return the new array type element
		 */
		TypeClass exp = (TypeClass) newArray.elements().Accept(this);
		Type arrayType = GetType(newArray.type(), newArray.arraydimension()+1,
				newArray.line(), newArray.type());
		if (!(arrayType instanceof ArrayType)) {
			CompError.message(newArray.line(), "This type  " + newArray.type()
					+ " was never created.");
		}
		if (newArray.arraydimension() < 0) {
			CompError.message(
					newArray.line(),
					"Negative numbers in arrays like this "
							+ newArray.arraydimension() + " Aren't allowed.");
		}
		
//		Type arrayT = new ArrayType(arrayType);
		
		AATExpression allocation = bt.allocate(new AATOperator(exp.value(), new AATConstant(MachineDependent.WORDSIZE), AATOperator.MULTIPLY));
		TypeClass returnClass = new TypeClass(arrayType, allocation);

		return returnClass;
	}

	// DONE
	@Override
	public Object VisitNewClassExpression(ASTNewClassExpression newClass) {
		/*
		 * Parameters: newClass
		 * 
		 * Types within the Parameters: String type, int line
		 * 
		 * Steps: Check to see if the type of the class is in the type
		 * environment Check to see if it is of class type If not Print an error
		 * Create a new class type with the variables of the class Return the
		 * new class type
		 */
		Type classType = typeEnv.find(newClass.type());
		if (!(classType instanceof ClassType)) {
			CompError.message(newClass.line(), "This class  " + newClass.type()
					+ " Has yet to be defined.");
		}
		ClassType classTypeType = (ClassType) classType;
		int classSize = classTypeType.variables().size();
		AATExpression allocation = bt
				.allocate(new AATConstant(classSize * 4));
		TypeClass returnClass = new TypeClass(classType, allocation);
		return returnClass;
	}

	// DONE
	@Override
	public Object VisitOperatorExpression(ASTOperatorExpression opExpr) {
		/*
		 * Parameters: opExpr
		 * 
		 * Types within the Parameters: ASTExpression left, ASTExpression right,
		 * int operator, int line
		 * 
		 * Steps: Accept the left Accept the right Check that the left and right
		 * sides are of the same type Use a switch case for all of the operators
		 * If not Print error Else Return null
		 */

		TypeClass left = (TypeClass) opExpr.left().Accept(this);
		TypeClass right = (TypeClass) opExpr.right().Accept(this);
		
		AATExpression leftv = left.value();
		AATExpression rightv = right.value();
		
		Type leftT = left.type();
		Type rightT = right.type();
		

		int sign = opExpr.operator();
		switch (sign) {
		case ASTOperatorExpression.PLUS:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(+) operator requires integer operands");
			}

			return new TypeClass(IntegerType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.PLUS));

		case ASTOperatorExpression.MINUS:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(-) operator requires integer operands");
			}
			return new TypeClass(IntegerType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.MINUS));

		case ASTOperatorExpression.MULTIPLY:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(*) operator requires integer operands");
			}
			return new TypeClass(IntegerType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.MULTIPLY));

		case ASTOperatorExpression.DIVIDE:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(/) operator requires integer operands");
			}
			return new TypeClass(IntegerType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.DIVIDE));

		case ASTOperatorExpression.AND:
			// if (lhs != IntegerType.instance() || rhs !=
			// IntegerType.instance()) {
			// CompError.message(opexpr.line(),
			// "(&&) operator requires integer operands");
			// }
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.AND));
		case ASTOperatorExpression.OR:
			// if (lhs != IntegerType.instance() || rhs !=
			// IntegerType.instance()) {
			// CompError.message(opexpr.line(),
			// "(||) operator requires integer operands");
			// }
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.OR));

		case ASTOperatorExpression.EQUAL:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(==) operator requires a boolean result");
			}
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.EQUAL));

		case ASTOperatorExpression.NOT_EQUAL:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(!=) operator requires a boolean result");
			}
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.NOT_EQUAL));

		case ASTOperatorExpression.LESS_THAN:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(<) operator requires a boolean result");
			}
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.LESS_THAN));

		case ASTOperatorExpression.LESS_THAN_EQUAL:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(<=) operator requires a boolean result");
			}
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.LESS_THAN_EQUAL));

		case ASTOperatorExpression.GREATER_THAN:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(>) operator requires a boolean result");
			}
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.GREATER_THAN));

		case ASTOperatorExpression.GREATER_THAN_EQUAL:
			if (leftT != IntegerType.instance()
					|| rightT != IntegerType.instance()) {
				CompError.message(opExpr.line(),
						"(>=) operator requires a boolean result");
			}
			return new TypeClass(BooleanType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.GREATER_THAN_EQUAL));

		default:
			return new TypeClass(IntegerType.instance(), bt.operatorExpression(
					leftv, rightv, AATOperator.BAD_OPERATOR));
		}
	}

	//TODO: DONE????
	@Override
	public Object VisitProgram(ASTProgram program) {
		/*
		 * Parameters: program
		 * 
		 * Types within the Parameters: ASTClasses classes,
		 * ASTFunctionDefinitions functiondefinitions, int line
		 * 
		 * Steps: Accept the classes Accept the function definitions Return null
		 */
		
		AATStatement statement, statement2 = bt.emptyStatement();
		TypeClass classes = (TypeClass) program.classes().Accept(this);
		statement = (AATStatement) program.functiondefinitions().Accept(this);
		return statement;
	}

	// TODO: DONE????
	@Override
	public Object VisitPrototype(ASTPrototype prototype) {
		/*
		 * Parameters: prototype
		 * 
		 * Types within the Parameters: String type, String name, ASTFormals
		 * formals, int line
		 * 
		 * Steps: Check to see if the prototype is in the function environment
		 * Check to see if the return type of the prototype is in the type
		 * environment Create a vector to store all of the arguments Loop
		 * through all of the arguments Accept each of them Add them to the
		 * vector If the return type wasn't found Create a new function entry
		 * Insert it into the function environment Else Print an error Return
		 * the entry
		 */
		FunctionEntry entry = funcEnv.find(prototype.name());
		
		Type type = typeEnv.find(prototype.type());

		Vector<Type> formals = new Vector<>();
		for (int i = 0; i < prototype.formals().size(); i++) {
			Type formalEnt = (Type) prototype.formals().elementAt(i)
					.Accept(this);
			formals.addElement(formalEnt);
		}
		if (entry == null) {
			entry = new FunctionEntry(type, formals, new Label(prototype.name()), new Label(prototype.name() + " end"));
			funcEnv.insert(prototype.name(), entry);
		} else {
			CompError.message(prototype.line(), "This  " + prototype.name()
					+ " is a duplicate." + entry);
		}
		return bt.emptyStatement();
	}

	// TODO: DONE????
	@Override
	public Object VisitReturnStatement(ASTReturnStatement ret) {
		/*
		 * Parameters: ret
		 * 
		 * Types within the Parameters: ASTExpression value, int line
		 * 
		 * Steps: Accept the return type Check that the return type is not empty
		 * If not Check to make sure that the return type is in the return
		 * variable environment If it is and it is not the same type as the
		 * value given Print an error Return null
		 */
		
		
		TypeClass value_ = (TypeClass) ret.value().Accept(this);
		
		if (value_.type() != null) {
			VariableEntry s = ret$.find("return");
			if (value_.type() != s.type()) {
				CompError.message(ret.line(), "These return type of " + value_
						+ " doesn't match the return type of " + s.type());
			}
			//move of value tree to result reg jump to end label
			AATMove newMove = new AATMove(new AATRegister(Register.Result()), value_.value());
			AATJump newJump = new AATJump(globalEnd);
			return bt.sequentialStatement(newMove, newJump);
		} else {
			CompError.message(ret.line(), "There are no valid expressions in  "
					+ value_ + ".");
		}
		return bt.emptyStatement();
	}

	// TODO: DONE????
	@Override
	public Object VisitStatements(ASTStatements statements) {
		/*
		 * Parameters: statements
		 * 
		 * Types within the Parameters: ASTStatement statement
		 * 
		 * Steps: Create a new variable scope Loop through all of the statements
		 * Accept each of the statements End the variable scope Return null
		 */
		AATStatement statement, statement2;
		AATStatement stat = bt.emptyStatement();
		varEnv.beginScope();
//		if(statements.size() == 1){
//			//single statement call accept then return it 
//			statement = (AATStatement)statements.elementAt(0).Accept(this);
//			stat = statement;
//		}
		statement2 = (AATStatement)statements.elementAt(0).Accept(this);
		for (int i = 1; i < statements.size(); i++) {
			//otherwise build bunch of seq statements
			statement = (AATStatement)statements.elementAt(i).Accept(this);
			statement2 = bt.sequentialStatement(statement2, statement);
		}
		varEnv.endScope();
		//0 return empty
		return statement2;
	}

	// DONE
	@Override
	public Object VisitUnaryOperatorExpression(ASTUnaryOperatorExpression operator) {
		/*
		 * Parameters: operator
		 * 
		 * Types within the Parameters: ASTExpression operand, int operator, int
		 * line
		 * 
		 * Steps: Accept the operator Check that the operator expression
		 * evaluates to a boolean value If not Print error Return null
		 */
		TypeClass expressionClass = (TypeClass) operator.operand().Accept(this);
		Type operand = (Type) operator.operand().Accept(this);
		TypeClass returnClass;
		if (operand != BooleanType.instance()) {
			CompError.message(operator.line(),
					" You are using the ! symbol improperly. ");
			returnClass = new TypeClass(IntegerType.instance(),
					(AATExpression) null);
		}
		AATExpression opExp = bt.operatorExpression(new AATConstant(1),
				expressionClass.value(), AATOperator.MINUS);
		returnClass = new TypeClass(BooleanType.instance(), opExp);
		return returnClass;
	}

	// TODO: DONE????
	@Override
	public Object VisitVariableDefStatement(ASTVariableDefStatement varDef) {
		/*
		 * Parameters: varDef
		 * 
		 * Types within the Parameters: String type, String name, int line OR
		 * String type, String name, ASTExpression init, int line OR String
		 * type, String name, int arraydimension, int line OR String type,
		 * String name, int arraydimension, ASTExpression init, int line
		 * 
		 * Steps: Get the type of the variable definition Create a new variable
		 * entry Insert it into the variable environment Check to see if there
		 * is an initialization statement If so Accept the type of the
		 * initialization statement Check to make sure the variable entry type
		 * and the type of the initialization statement are the same If not
		 * Print an error Return null
		 */
		
		//if no init then empty statement 
		//else assignment move etc
		//global set 0
		
		Type varDefGuy = GetType(varDef.type(), varDef.arraydimension(),
				varDef.line(), varDef.name());
		
		VariableEntry newGuy = new VariableEntry(varDefGuy, funcOffset);
		
		//add 4 to global
		funcOffset += 4;
		varEnv.insert(varDef.name(), newGuy);
		if (varDef.init() != null) {
			TypeClass varDefInit = (TypeClass) varDef.init().Accept(this);
			if (varDefInit.type() != varDefGuy) {
				CompError.message(varDef.line(), "This type " + varDefGuy
						+ " is not the same as this type " + varDefInit.type());
			}
			//memory(of fp - offset * wordsize)
			return bt.assignmentStatement(bt.baseVariable(newGuy.offset()), varDefInit.value());
		}
		//return callstatement with either empty or the assignment 
		return bt.emptyStatement();
	}

	// DONE
	@Override
	public Object VisitVariableExpression(ASTVariableExpression variableExpression) {
		/*
		 * Parameters: variableExpression
		 * 
		 * Types within the Parameters: ASTVariable variable, int line
		 * 
		 * Steps: Accept the variable Check to see if the variable is null If so
		 * Print an error Return null
		 */
		TypeClass variable_ = (TypeClass) variableExpression.variable().Accept(
				this);
		if (variable_ == null) {
			CompError
					.message(variableExpression.line(), "The variable  "
							+ variableExpression.variable().toString()
							+ " is invalid.");
		}
		return variable_;
	}

	// DONE
	@Override
	public Object VisitWhileStatement(ASTWhileStatement whileStatement) {
		/*
		 * Parameters: whileStatement
		 * 
		 * Types within the Parameters: ASTExpression test, ASTStatement body,
		 * int line
		 * 
		 * Steps: Accept the test of the while Check to see if the test
		 * evaluates to a boolean value If not Print an error Accept the body of
		 * the while Return null
		 */
		TypeClass test = (TypeClass) whileStatement.test().Accept(this);
		if (test.type() != BooleanType.instance()) {
			CompError.message(whileStatement.line(), " This test " + test
					+ " does not evaluate to a boolean value.");
		}
		AATStatement body = (AATStatement) whileStatement.body().Accept(this);
		AATStatement returnWhile = bt.whileStatement(test.value(), body);

		return returnWhile;
	}

	//DONE
	public Type GetType(String type, int size, int line, String name) {
		/*
		 * Parameters: type, size, line, name
		 * 
		 * Types within the Parameters: String, int, int, String
		 * 
		 * Steps: Check that the type is in the type environment Check to see if
		 * the size is zero If not Return the type Else Print an error Else
		 * Create a string to modify the type Loop through the size Add on []
		 * for each iteration Check if the string is in the type environment If
		 * not Create a new Array type and recursively call the function again
		 * Insert that item into the type environment after recursion completes
		 * Return the new item
		 */
		//System.out.println(size);
		Type thing = typeEnv.find(type);
		if (size == 0) {
			if (thing != null) {
				return thing;
			} else {
				CompError.message(line, name + " this type is undefined.");
			}
		} else {
			String temp = type;
			for (int i = 0; i < size; i++) {
				temp += "[]";
			}
			thing = typeEnv.find(temp);
			if (thing == null) {
				thing = new ArrayType(GetType(type, size - 1, line, name));
				typeEnv.insert(temp, thing);
				thing = typeEnv.find(temp);
			}
		}
		//return new Type(thing,offset);
		return thing;
	}

}