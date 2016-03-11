import java.util.Vector;

public class AATBuildTree {
	
	Label alloc = new Label("allocate");

	// DONE
	public AATExpression allocate(AATExpression size) {
		/*
		 * Allocate creates an AAT that makes a CALL to the built-in function
		 * allocate, which takes as input the size (in bytes) to allocate, and
		 * returns a pointer to the beginning of the allocated block
		 */
		Vector<AATExpression> actuals = new Vector<>();
		if (actuals.add(size)) {
			return new AATCallExpression(
					Label.AbsLabel("allocate"), actuals);
		}
		return null;

	}

	// DONE
	public AATExpression arrayVariable(AATExpression base, AATExpression index,
			int elementSize) {
		// elem size is the size is constant?
		AATConstant elemSizeCo = new AATConstant(elementSize);
		AATOperator elemIndexOp = new AATOperator(elemSizeCo, index,
				AATOperator.MULTIPLY);
		AATMemory baseMem = new AATMemory(base);
		AATOperator baseIndexOp = new AATOperator(base, elemIndexOp,
				AATOperator.MINUS);
		return new AATMemory(baseIndexOp);
	}

	// DONE
	public AATStatement assignmentStatement(AATExpression lhs, AATExpression rhs) {
		return new AATMove(lhs, rhs);
	}

	// DONE
	// .|
	// (-)
	// / \
	// FP offset*Wordsize
	public AATExpression baseVariable(int offset) {
		return new AATMemory(new AATOperator(new AATRegister(Register.FP()),
				new AATConstant(offset), AATOperator.MINUS));
	}

	// DONE
	// need something like this after Vector to get rid of the yellow screaming
	// <ASTFormals>
	public AATExpression callExpression(Vector actuals, Label name) {
		return new AATCallExpression(name, actuals);
	}

	// DONE
	// need something like this after Vector to get rid of the yellow screaming
	// <ASTFormals>
	public AATStatement callStatement(Vector actuals, Label name) {
		return new AATCallStatement(name, actuals);
	}

	// DONE
	public AATExpression classVariable(AATExpression base, int offset) {
		AATConstant offsetCon = new AATConstant(offset);
		AATOperator baseOffsetOp = new AATOperator(base, offsetCon,
				AATOperator.MINUS);
		return new AATMemory(baseOffsetOp);
	}

	// DONE
	public AATExpression constantExpression(int value) {
		return new AATConstant(value);
	}

	// DONE
	public AATStatement dowhileStatement(AATExpression test,
			AATStatement dowhilebody) {

		Label doWhileStartLabel = new Label();
		Label doWhileTestLabel = new Label();
		Label doWhileEndLabel = new Label();

		AATLabel doWhileStart = new AATLabel(doWhileStartLabel);

		AATLabel doWhileTest = new AATLabel(doWhileTestLabel);

		AATLabel doWhileEnd = new AATLabel(doWhileEndLabel);

		AATConditionalJump jump = new AATConditionalJump(test,
				doWhileStartLabel);

		AATStatement seq = sequentialStatement(doWhileTest, jump);

		seq = sequentialStatement(dowhilebody, seq);

		seq = sequentialStatement(doWhileEnd, seq);

		AATJump jumpTest = new AATJump(doWhileTestLabel);

		seq = sequentialStatement(jumpTest, seq);

		seq = sequentialStatement(dowhilebody, seq);

		return seq;

	}

	// DONE
	public AATStatement emptyStatement() {
		return new AATEmpty();
	}

	// DONE
	public AATStatement forStatement(AATStatement init, AATExpression test,
			AATStatement increment, AATStatement body) {

		// FOR_test label
		Label forTestLabel = new Label("FOR_test");
		AATLabel forTest = new AATLabel(forTestLabel);

		// FOR_start label/jump
		Label forStartLabel = new Label("FOR_start");
		AATConditionalJump forStartJump = new AATConditionalJump(test,
				forStartLabel);

		AATStatement seq = sequentialStatement(forTest, forStartJump);
		seq = sequentialStatement(increment, seq);
		seq = sequentialStatement(body, seq);

		// For_start label location
		AATLabel forStart = new AATLabel(forStartLabel);
		seq = sequentialStatement(forStart, seq);

		// FOR_Test jump location
		AATJump forTestJump = new AATJump(forTestLabel);
		seq = sequentialStatement(forTestJump, seq);
		seq = sequentialStatement(init, seq);

		return seq;
	}

	// TODO: Done??
	public AATStatement functionDefinition(AATStatement body, int framesize,
			Label start, Label end) {
		/*
		 * //body: The body of the function framesize: The size of the frame
		 * devoted to local variables start: The assembly language label for the
		 * start of the function end: The assembly language label for use by
		 * return statements (to appear just before the cleanup code)
		 */

		int wordSize = MachineDependent.WORDSIZE;
		
		AATLabel fDStart = new AATLabel(start);
		AATLabel fDEnd = new AATLabel(end);

		AATReturn ret = new AATReturn();
		
		int little = framesize;
		int middle = framesize + wordSize;
		int big = framesize + (2 * wordSize);
		
		//changed this to result
		AATRegister returnAddress = new AATRegister(Register.ReturnAddr());
		
		//created restore spaces
		AATMemory restoreSP = new AATMemory(new AATOperator(new AATRegister(Register.FP()), new AATConstant(little), AATOperator.MINUS));
		AATMemory restoreFP = new AATMemory(new AATOperator(new AATRegister(Register.FP()), new AATConstant(middle), AATOperator.MINUS));
		AATMemory restoreRA = new AATMemory(new AATOperator(new AATRegister(Register.FP()),new AATConstant(big), AATOperator.MINUS));

		//created save spaces and changed to minus because we want SP - little big or middle since it needs to go below the SP not above
		AATMemory saveRA = new AATMemory(new AATOperator(new AATRegister(Register.SP()), new AATConstant(big), AATOperator.MINUS));
		AATMemory saveFP = new AATMemory(new AATOperator(new AATRegister(Register.SP()), new AATConstant(middle), AATOperator.MINUS));
		AATMemory saveSP = new AATMemory(new AATOperator(new AATRegister(Register.SP()), new AATConstant(little), AATOperator.MINUS));
				
		
		//restore FP -- 10
		AATStatement seq = sequentialStatement(new AATMove(new AATRegister(Register.FP()), restoreFP), ret);
		
		//restore SP -- 9
		seq = sequentialStatement(new AATMove(new AATRegister(Register.SP()), restoreSP), seq);
		
		//put val into RA -- 8
		seq = sequentialStatement(new AATMove(returnAddress, restoreRA), seq);
		
		//end label stuff -- 7
		seq = sequentialStatement(fDEnd, seq);
		
		//body stuff -- 6
		seq = sequentialStatement(body, seq);

		//move SP -- 5
		seq = sequentialStatement(new AATMove(new AATRegister(Register.SP()),new AATOperator(new AATRegister(Register.SP()), new AATConstant(big + wordSize), AATOperator.MINUS)), seq);
		
		//move FP -- 4
		seq = sequentialStatement(new AATMove(new AATRegister(Register.FP()), new AATRegister(Register.SP())), seq);
		
		//save RA -- 3
		seq = sequentialStatement(new AATMove(saveRA, new AATRegister(Register.ReturnAddr())), seq);
		
		//save FP -- 2
		seq = sequentialStatement(new AATMove(saveFP, new AATRegister(Register.FP())), seq);
		
		//save SP -- 1
		seq = sequentialStatement(new AATMove(saveSP, new AATRegister(Register.SP())), seq);
		
		//start label stuff -- 0
		seq = sequentialStatement(fDStart, seq);
		
		return seq;

	}

	// DONE
	public AATStatement ifStatement(AATExpression test, AATStatement ifbody,
			AATStatement elsebody) {
		// -//-//-//-//-//-//-//-//-//-//-//
		// test: An AAT, representing the test of the if statement
		// ifbody: An AAT, representing the <if> portion of the statement
		// elsebody: An AAT, representing the <else> portion of the statement
		// (could be empty)
		// -//-//-//-//-//-//-//-//-//-//-//

		Label ifEndLabel = new Label();
		Label ifTrueLabel = new Label();

		AATLabel ifTrue = new AATLabel(ifTrueLabel);

		AATLabel ifEnd = new AATLabel(ifEndLabel);
		// seq
		// first lower/inner sequence (ifBody ^ ifEnd);
		AATStatement seq = sequentialStatement(ifbody, ifEnd);

		// seq
		// second lower/inner sequence (ifTrue ^ seq);
		seq = sequentialStatement(ifTrue, seq);

		AATJump ifEndJump = new AATJump(ifEndLabel);
		// seq
		// third middle/inner sequence ( ifEndJump ^ seq);
		seq = sequentialStatement(ifEndJump, seq);

		// seq
		// fourth middle/inner sequence (elseBody ^ seq);
		
		if (elsebody != null) {
			seq = sequentialStatement(elsebody, seq);
		}

		AATConditionalJump ifTrueJump = new AATConditionalJump(test,
				ifTrueLabel);
		// seq
		// fifth outter sequence (ifTrueJump ^ seq);
		seq = sequentialStatement(ifTrueJump, seq);

		return seq;
	}

	// DONE
	public AATExpression operatorExpression(AATExpression left,
			AATExpression right, int operator) {
		return new AATOperator(left, right, operator);

	}

	public AATStatement returnStatement(AATExpression value, Label functionend) {
		/*
		 * value: The value to return functionend: The label to jump to, after
		 * the Result register has been set to value
		 */
		AATRegister resultRegister = new AATRegister(Register.Result());
		AATMove saveResult = new AATMove(resultRegister, value);
		AATJump endJump = new AATJump(functionend);

		return sequentialStatement(saveResult, endJump);

	}

	// DONE
	public AATStatement sequentialStatement(AATStatement first,
			AATStatement second) {
		return new AATSequential(first, second);
	}

	// DONE
	public AATStatement whileStatement(AATExpression test,
			AATStatement whilebody) {

		Label whileStartLabel = new Label();
		Label whileTestLabel = new Label();
		Label whileEndLabel = new Label();

		AATLabel whileStart = new AATLabel(whileStartLabel);

		AATLabel whileTest = new AATLabel(whileTestLabel);

		AATLabel whileEnd = new AATLabel(whileEndLabel);

		AATConditionalJump jump = new AATConditionalJump(test, whileStartLabel);

		AATStatement seq = sequentialStatement(whileTest, jump);

		seq = sequentialStatement(whilebody, seq);

		seq = sequentialStatement(whileStart, seq);

		AATJump jumpTest = new AATJump(whileTestLabel);

		seq = sequentialStatement(jumpTest, seq);

		return seq;
	}
}