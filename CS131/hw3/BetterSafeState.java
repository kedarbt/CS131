import java.util.concurrent.locks.ReentrantLock;

class BetterSafeState implements State {
    private byte[] value;
    private byte maxval;
    private ReentrantLock betterLock; // reentrant lock keeps swap thread safe

    BetterSafeState(byte[] v) { 
    	value = v; maxval = 127;
    	betterLock = new ReentrantLock(); //create lock
     }

    BetterSafeState(byte[] v, byte m) {  
    	maxval = m;
    	value = v;
    	betterLock = new ReentrantLock();
    }

    public int size() { return value.length; }

    public byte[] current() {
    	return value;
 	}

    public boolean swap(int i, int j) {
    	betterLock.lock(); //lock before swapping
		if (value[i] <= 0 || value[j] >= maxval) {
			betterLock.unlock(); //unlock if swap wont work
	    	return false;
		}
		value[i]--;
		value[j]++;
		betterLock.unlock(); //unlock after swapping
		return true;
    }
}