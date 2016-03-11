class TypeClass {
	Type type_;
	AATExpression value_;

	public TypeClass(Type type, AATExpression value) {
		type_ = type;
		value_ = value;
	}

	public Type type() {
		return type_;
	}

	public AATExpression value() {
		return value_;
	}

	public void settype(Type type) {
		type_ = type;
	}
}