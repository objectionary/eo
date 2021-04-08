package org.eolang.core;

import org.eolang.core.data.EOData;
import org.eolang.core.data.EODataObject;

import java.lang.reflect.Method;
import java.util.Optional;


/**
 * Basic EO object. Based on this class, classes are created for creating user objects.
 */
public abstract class EOObject implements Cloneable {
    /**
     * Link to the parent of the object
     */
    protected EOObject _parent;

    /**
     * Checking if an object can be datarized when creating a datarized object for caching.
     * A variant of solving the problem of exponential growth of datarization time during recursion.
     *
     * @return the boolean
     */
    public boolean _isCalculable() {
        return false;
    }

    public EOObject _getDecoratedObject() {
        return null;
    }

    public EOObject _getParentObject() {
        return null;
    }

    /**
     * Setting the parent object.
     *
     * @param _parent The parent object
     * @return this
     */
    public EOObject _setParent(EOObject _parent) {
        if (this._parent == null)
            this._parent = _parent;
        return this;
    }

    /**
     * Function that performs dateization of the object
     *
     * @return Data
     */
    public EOData _getData() {
        EOObject decoratee = _getDecoratedObject();
        if (decoratee == null) {
            throw new RuntimeException("Object cannot be dataized.");
        }
        return decoratee._getData();
    }

    public EOObject _getAttribute(String name, EOObject... freeAtt) {
        try {
            Method method = this.getClass().getDeclaredMethod("EO" + name, EOObject.class);
            method.setAccessible(true);
            return (EOObject) method.invoke(this, freeAtt);
        } catch (Exception e) {
            throw new RuntimeException(String.format("Can't access the %s attribute of the %s object", name, this.getClass().getName()));
        }
    }

    public EOObject _getAttribute(String name) {
        try {
            Method method = this.getClass().getDeclaredMethod(name);
            method.setAccessible(true);
            return (EOObject) method.invoke(this, null);
        } catch (Exception e) {
            throw new RuntimeException(String.format("Can't access the %s attribute of the %s object", name, this.getClass().getName()));
        }
    }
}
