class ArrayType extends Type {

    public ArrayType(Type type) {
	type_ = type;
    }

    public Type type() {
	return type_;
    }

    public void settype(Type type) {
	type_ = type;
    }
    
    public String toString(){
    	return "Array is " + type_.toString();
    }

    private Type type_;
}