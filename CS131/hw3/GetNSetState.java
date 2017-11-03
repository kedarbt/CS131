import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private byte maxval;
    private AtomicIntegerArray value;

	private void createAtomicArray(byte[] v){
    	int[] atomic_construct = new int[v.length];
    	for(int i = 0; i < v.length; i++){
    		atomic_construct[i] = v[i];
		}
		value = new AtomicIntegerArray(atomic_construct);
	}


    GetNSetState(byte[] v) { 
    	maxval = 127;
    	createAtomicArray(v);
    }

    GetNSetState(byte[] v, byte m) { 
    	maxval = m;
    	createAtomicArray(v);
    }

    public int size() { return value.length(); }

    public byte[] current() { 
		byte[] byte_construct = new byte[value.length()];
		for(int i = 0; i < byte_construct.length; i++){
			byte_construct[i] = (byte) value.get(i);
		}
		return byte_construct;
	}


	public boolean swap(int i, int j) {
		if (value.get(i) <= 0 || value.get(j) >= maxval) {
			return false;
		}
		value.getAndDecrement(i);
		value.getAndIncrement(j);
		return true;
	}
}