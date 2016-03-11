import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

class CodeGenerator implements AATVisitor {

	private static final int ws = MachineDependent.WORDSIZE;

	public CodeGenerator(String output_filename) {
		try {
			output = new PrintWriter(new FileOutputStream(output_filename));
		} catch (IOException e) {
			System.out.println("Could not open file " + output_filename
					+ " for writing.");
		}
		/* Feel free to add code here, if you want to */
		EmitSetupCode();
	}

	@Override
	public Object VisitCallExpression(AATCallExpression expression) {
		int actualSize = expression.actuals().size();
		for (int i = actualSize; i > 0; i--) {
			// sw $ACC/$t0, -(ws*i-ws)($sp)
			emit("sw\t\t" + Register.ACC() + ", " + (0 - ((ws * i) - ws)) + "("
					+ Register.SP() + ")" + "\t # Storing actual #" + i);
		}
		// addi $sp, $sp, -(wordSize * actuals)
		emit("addi\t" + Register.SP() + ", " + Register.SP() + ", "
				+ ((0 - ws) * actualSize) + "\t\t # Incrementing Stack Pointer");
		// jal label
		emit("jal\t" + expression.label() + "\t\t\t\t # Jumping to "
				+ expression.label().toString());
		// addi $sp, $sp, (wordsize * actuals)
		emit("addi\t" + Register.SP() + ", " + Register.SP() + ", "
				+ (ws * actualSize) + "\t\t # Decrementing Stack Pointer");
		// addi $ACC/$t0, $result/$v0, 0
		emit("addi\t" + Register.ACC() + ", " + Register.Result() + ", " + 0
				+ "\t\t # Storing Result into Accumulator");

		return null;
	}

	@Override
	public Object VisitCallStatement(AATCallStatement statement) {
		int actualSize = statement.actuals().size();
		for (int i = actualSize; i > 0; i--) {
			// sw $ACC/$t0, -(ws*i-ws)($sp)
			emit("sw\t\t" + Register.ACC() + ", " + (0 - ((ws * i) - ws)) + "("
					+ Register.SP() + ")");
		}
		// addi $sp, $sp, -(wordSize * actuals)
		emit("addi\t" + Register.SP() + ", " + Register.SP() + ", "
				+ ((0 - ws) * actualSize));
		// jal label
		emit("jal\t" + statement.label());
		// addi $sp, $sp, (wordsize * actuals)
		emit("addi\t" + Register.SP() + ", " + Register.SP() + ", "
				+ (ws * actualSize));

		return null;
	}

	@Override
	public Object VisitConditionalJump(AATConditionalJump statement) {
		statement.test().Accept(this);
		// bgtz $ACC/$t0 label
		emit("bgtz\t" + Register.ACC() + statement.label());

		return null;
	}

	@Override
	public Object VisitConstant(AATConstant expression) {
		// li $ACC/$t0, constant
		emit("li\t\t" + Register.ACC() + ", " + expression.value()
				+ "\t\t\t # Loading Constant value");

		return null;
	}

	@Override
	public Object VisitEmpty(AATEmpty statement) {
		return null;
	}

	@Override
	public Object VisitHalt(AATHalt halt) {
		/*
		 * Don't need to implement halt -- you can leave this as it is, if you
		 * like
		 */
		return null;
	}

	@Override
	public Object VisitJump(AATJump statement) {
		// j label
		emit("j " + statement.label());
		return null;
	}

	@Override
	public Object VisitLabel(AATLabel statement) {
		// label:
		emit(statement.label() + ":");
		return null;
	}

	@Override
	public Object VisitMemory(AATMemory expression) {
		expression.mem().Accept(this);
		// lw $ACC/$t0, 0($ACC/$t0)
		emit("lw\t\t" + Register.ACC() + ", 0(" + Register.ACC() + ")");

		return null;
	}

	@Override
	public Object VisitMove(AATMove statement) {
		// if moving to a register in left side of tree
		if (statement.lhs() instanceof AATRegister) {
			statement.rhs().Accept(this);

			AATRegister reg_ = (AATRegister) statement.lhs();
			Register lhs_register = reg_.register();

			// addi $lhs_register , $ACC/$t0, 0
			emit("addi\t" + lhs_register + ", " + Register.ACC() + ", " + 0);

			// else moving to a memory locatino in left side of tree
		} else {
			AATMemory mem_ = (AATMemory) statement.lhs();
			mem_.mem().Accept(this);

			// sw $ACC/$t0 , 0($ESP)
			emit("sw\t\t" + Register.ACC() + ", " + 0 + "(" + Register.ESP()
					+ ")");
			// addi $ESP, $ESP, -wordSize
			emit("addi\t" + Register.ESP() + ", " + Register.ESP() + ", "
					+ (0 - ws));

			statement.rhs().Accept(this);

			// lw $Tmp1/$t2 , wordSize($ESP)
			emit("lw\t\t" + Register.Tmp1() + ", " + ws + "(" + Register.ESP()
					+ ")");
			// sw $ACC/$t0, 0($Tmp1/$t2)
			emit("sw\t\t" + Register.ACC() + ", " + 0 + "(" + Register.Tmp1()
					+ ")");

		}

		return null;
	}

	@Override
	public Object VisitOperator(AATOperator expression) {
		expression.left().Accept(this);

		// sw $ACC/$t0, 0($ESP)
		emit("sw\t\t"
				+ Register.ACC()
				+ ", "
				+ 0
				+ "("
				+ Register.ESP()
				+ ")"
				+ "\t\t # Storing Accumulator value onto Expression Stack Pointer.");
		// addi $ESP, $ESP, -wordSize
		emit("addi\t" + Register.ESP() + ", " + Register.ESP() + ", "
				+ (0 - ws) + "\t # Decrementing Expression Stack Pointer.");

		expression.right().Accept(this);

		// lw $Tmp1/$t2, wordSize($ESP)
		emit("lw\t\t"
				+ Register.Tmp1()
				+ ", "
				+ ws
				+ "("
				+ Register.ESP()
				+ ")"
				+ "\t\t # Loading Expression Stack Pointer Value into Temporary Register.");
		// addi $ESP, $ESP, wordSize
		emit("addi\t" + Register.ESP() + ", " + Register.ESP() + ", " + ws
				+ "\t\t # Incrementing Expression Stack Pointer.");

		switch (expression.operator()) {
		case AATOperator.PLUS:
			// add $ACC/$t0, $Tmp1/$t2, $ACC/$t0
			emit("add\t\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC() + "\t # $t0 = $t2 + $t0");
			break;

		case AATOperator.MINUS:
			// sub $ACC/$t0, $Tmp1/$t2, $ACC/$t0
			emit("sub\t\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC() + "\t # $t0 = $t2 - $t0");
			break;

		case AATOperator.MULTIPLY:
			// mult $ACC/$t0, $Tmp1/$t2, $ACC/$t0
			emit("mult\t\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC() + "\t # $t0 = $t2 x $t0");

			break;
		case AATOperator.DIVIDE:
			// div $ACC/$t0, $Tmp1/$t2, $ACC/$t0
			emit("div\t\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC() + "\t # $t0 = $t2 / $t0");

			break;

		case AATOperator.AND:
			// 1 < x + y ? return 1 : return 0
			// add $ACC/$t0, $Tmp1/$t2, $ACC/$t0
			emit("add\t\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC());
			// slt $ACC/$t0, 1, $ACC/$t0
			emit("slt\t" + Register.ACC() + ", " + 1 + ", " + Register.ACC()
					+ "\t\t # $t0 && $t2");

			break;

		case AATOperator.OR:
			// 0 < x + y ? return 1 : return 0
			// add $ACC/$t0, $Tmp1/$t2, $ACC/$t0
			emit("add\t\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC());
			// slt $ACC/$t0, 0, $ACC/$t0
			emit("slt\t" + Register.ACC() + ", " + 0 + ", " + Register.ACC()
					+ "\t\t # $t0 || $t2");

			break;

		case AATOperator.NOT:
			// addi $Tmp1/$t2, 0, 1
			emit("addi\t" + Register.Tmp1() + ", " + 0 + ", " + 1);
			// sub $ACC/$t0, $Tmp1/$t2 $ACC/$t0
			emit("sub\t\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC() + "\t # $t0 != $t2");

			break;

		case AATOperator.GREATER_THAN_EQUAL:
			// addi $Tmp1/$t2, $Tmp1/$t2, -1
			emit("addi\t" + Register.Tmp1() + ", " + Register.Tmp1() + ", "
					+ -1);
			// slt $ACC/$t0, $ACC/$t0, $Tmp1/$t2
			emit("slt\t" + Register.ACC() + ", " + Register.ACC() + ", "
					+ Register.Tmp1() + "\t\t # $t0 >= $t2");

			break;

		case AATOperator.GREATER_THAN:
			// slt $ACC/$t0, $ACC/$t0, $Tmp1/$t2
			emit("slt\t" + Register.ACC() + ", " + Register.ACC() + ", "
					+ Register.Tmp1() + "\t\t # $t0 > $t2");
			break;

		case AATOperator.EQUAL:
			// slt $Tmp2/$t3, $Tmp1/$t2, $ACC/$t0
			emit("slt\t" + Register.Tmp2() + ", " + Register.Tmp1() + ", "
					+ Register.ACC());
			// slt $ACC/$t0 $ACC/$t0 $Tmp1
			emit("slt\t" + Register.ACC() + ", " + Register.ACC() + ", "
					+ Register.Tmp1());
			// add $ACC/$t0 $Tmp2/$t3, $ACC/$t0
			emit("add\t\t" + Register.ACC() + ", " + Register.Tmp2() + ", "
					+ Register.ACC());
			// sub $ACC/$t0 0, $ACC/$t0
			emit("sub\t\t" + Register.ACC() + ", " + 0 + ", " + Register.ACC());
			// addi $ACC/$t0 $ACC/$t0 1
			emit("addi\t" + Register.ACC() + ", " + Register.ACC() + ", " + 1);

			break;

		case AATOperator.NOT_EQUAL:
			// slt $Tmp2/$t3, $Tmp1/$t2, $ACC/$t0
			emit("slt\t" + Register.Tmp2() + ", " + Register.Tmp1() + ", "
					+ Register.ACC());
			// slt $ACC/$t0 $ACC/$t0 $Tmp1
			emit("slt\t" + Register.ACC() + ", " + Register.ACC() + ", "
					+ Register.Tmp1());
			// add $ACC/$t0 $Tmp2/$t3, $ACC/$t0
			emit("add\t\t" + Register.ACC() + ", " + Register.Tmp2() + ", "
					+ Register.ACC());
			// slt $ACC/$t0 0, $ACC/$t0
			emit("addi\t" + Register.ACC() + ", " + 0 + ", " + Register.ACC());
			break;

		case AATOperator.LESS_THAN:
			// slt $ACC/$t0, $Tmp1/$t2, $ACC/$t0
			emit("slt\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC() + "\t\t # $t0 < $t2");
			break;

		case AATOperator.LESS_THAN_EQUAL:
			// addi $Tmp1/$t2, $Tmp1/$t2, -1
			emit("addi\t" + Register.Tmp1() + ", " + Register.Tmp1() + ", "
					+ -1);
			// slt $ACC/$t0, $Tmp1/$t2 , $ACC/$t0
			emit("slt\t" + Register.ACC() + ", " + Register.Tmp1() + ", "
					+ Register.ACC() + "\t\t # $t0 <= $t2");
			break;

		}

		return null;
	}

	@Override
	public Object VisitRegister(AATRegister expression) {
		Register $r1 = expression.register();
		// addi $ACC/$t0 , $r1, 0
		emit("addi\t" + Register.ACC() + ", " + $r1 + ", " + 0
				+ " \t # Loading Register into Accumulator");
		return null;
	}

	@Override
	public Object VisitReturn(AATReturn statement) {
		// jr returnAddr
		emit("jr " + Register.ReturnAddr()
				+ "\t\t\t\t\t # Jump Return back to "
				+ Register.ReturnAddr().toString());
		return null;
	}

	@Override
	public Object VisitSequential(AATSequential statement) {
		statement.left().Accept(this);
		statement.right().Accept(this);

		return null;
	}

	private void emit(String assem) {
		assem = assem.trim();
		if (assem.charAt(assem.length() - 1) == ':')
			output.println(assem);
		else
			output.println("\t" + assem);
	}

	public void GenerateLibrary() {
		emit("Print:");
		emit("lw $a0, 4(" + Register.SP() + ")");
		emit("li $v0, 1");
		emit("syscall");
		emit("li $v0,4");
		emit("la $a0, sp");
		emit("syscall");
		emit("jr $ra");
		emit("Println:");
		emit("li $v0,4");
		emit("la $a0, cr");
		emit("syscall");
		emit("jr $ra");
		emit("Read:");
		emit("li $v0,5");
		emit("syscall");
		emit("jr $ra");
		emit("allocate:");
		emit("la " + Register.Tmp1() + ", HEAPPTR");
		emit("lw " + Register.Result() + ",0(" + Register.Tmp1() + ")");
		emit("lw " + Register.Tmp2() + ", 4(" + Register.SP() + ")");
		emit("sub " + Register.Tmp2() + ", " + Register.Result() + ", "
				+ Register.Tmp2());
		emit("sw " + Register.Tmp2() + ",0(" + Register.Tmp1() + ")");
		emit("jr $ra");
		emit(".data");
		emit("cr:");
		emit(".asciiz \"\\n\"");
		emit("sp:");
		emit(".asciiz \" \"");
		emit("HEAPPTR:");
		emit(".word 0");
		output.flush();
	}

	private void EmitSetupCode() {
		emit(".globl main");
		emit("main:");
		emit("addi " + Register.ESP() + ", " + Register.SP() + ",0");
		emit("addi " + Register.SP() + ", " + Register.SP() + ", "
				+ -MachineDependent.WORDSIZE * STACKSIZE);
		emit("addi " + Register.Tmp1() + ", " + Register.SP() + ",0");
		emit("addi " + Register.Tmp1() + ", " + Register.Tmp1() + ", "
				+ -MachineDependent.WORDSIZE * STACKSIZE);
		emit("la " + Register.Tmp2() + ", HEAPPTR");
		emit("sw " + Register.Tmp1() + ",0(" + Register.Tmp2() + ")");
		emit("sw " + Register.ReturnAddr() + ", " + MachineDependent.WORDSIZE
				+ "(" + Register.SP() + ")");
		emit("jal main1");
		emit("li $v0, 10");
		emit("syscall");
	}

	private final int STACKSIZE = 1000;
	private PrintWriter output;
	/* Feel Free to add more instance variables, if you like */
}