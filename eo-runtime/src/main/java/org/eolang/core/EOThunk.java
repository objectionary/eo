package org.eolang.core;

import org.eolang.core.data.EOData;
import java.util.function.Supplier;

public class EOThunk extends EOObject {

    private EOObject computed;

    private Supplier<EOObject> thunk;

    public EOThunk(Supplier<EOObject> thunk) {
        this.thunk = thunk;
    }

    private void compute() {
        computed = thunk.get();
    }

    @Override
    public EOObject _getDecoratedObject() {
        if (computed == null) {
            compute();
        }
        return computed._getDecoratedObject();
    }

    @Override
    public EOObject _getParentObject() {
        if (computed == null) {
            compute();
        }
        return computed._getParentObject();
    }

    /**
     * Function that performs dataization of the object
     * @return Data
     */
    @Override
    public EOData _getData() {
        if (computed == null) {
            compute();
        }
        return computed._getData();
    }

    @Override
    public EOObject _getAttribute(String name, EOObject... freeAtt) {
        if (computed == null) {
            compute();
        }
        return computed._getAttribute(name, freeAtt);
    }
}
