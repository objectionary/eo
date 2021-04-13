package org.eolang.core;

import org.eolang.core.data.EOData;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;


/**
 * Basic EO object. Based on this class, classes are created for creating user objects.
 */
public abstract class EOObject implements Cloneable {

    public EOObject _getDecoratedObject() {
        return null;
    }

    public EOObject _getParentObject() {
        return null;
    }

    /**
     * Function that performs dataization of the object
     * @return Data
     */
    public EOData _getData() {
        EOObject decoratee = _getDecoratedObject();
        if (decoratee == null) {
            throw new RuntimeException("Object cannot be dataized.");
        }
        return decoratee._getData();
    }

    private Object[] _prepareFreeAtt(Parameter[] methodParams, EOObject... freeAtt){
        List<Object> methodValues = new ArrayList<Object>();
        for(int i=0; i<methodParams.length; i++){
            if(methodParams[i].getType().getCanonicalName().endsWith("[]")){
                List<EOObject> objs = Arrays.stream(freeAtt).skip(i).collect(Collectors.toList());
                methodValues.add(objs.toArray(new EOObject[0]));
                break;
            }else{
                methodValues.add(freeAtt[i]);
            }
        }
        return methodValues.toArray();
    }

    public EOObject _getAttribute(String name, EOObject... freeAtt) {
        try {
            Method method = Arrays.stream(this.getClass().getMethods()).filter(mthd -> mthd.getName().equals(name)).findFirst().get();
            Parameter[] methodParams = method.getParameters();
            method.setAccessible(true);
            return (EOObject) method.invoke(this, _prepareFreeAtt(methodParams, freeAtt));
        } catch (Exception e) {
            if (this._getDecoratedObject() != null && this._getDecoratedObject() != this) {
                return _getDecoratedObject()._getAttribute(name, freeAtt);
            }
            else {
                e.printStackTrace();
                throw new RuntimeException(String.format("Can't access the %s attribute of the %s object", name, this.getClass().getName()));
            }
        }
    }
}
