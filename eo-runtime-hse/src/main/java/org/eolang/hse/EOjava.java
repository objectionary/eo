package org.eolang.hse;

import org.eolang.hse.core.EOObject;
import org.eolang.hse.core.data.EODataObject;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


/**
 * Wrapper for Java classes
 * Turns a java class instance or method into an EOObject
 */
public class EOjava extends EOObject {

    /**
     * creates a new java class instance and turns it into an {@code EOObject}
     * @param javaClass a EO string object representing the specific java class to instantiate
     * @return An {@code EOObject} resenting the Java class instance
     */
    public EOObject EOnew(EOObject javaClass){
        EOObject result = new EOstring();
        try{
            Class<?> cls = Class.forName(javaClass._getData().toString());
            Constructor<?> constructor = cls.getConstructor();
            result = new EODataObject(constructor.newInstance().toString());
            return result;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }

    /**
     * creates a new java class instance and turns it into an {@code EOObject}
     * @param javaClass a EO string object representing the specific java class to instantiate
     * @param a An EO integer object representing the argument for the constructor
     * @return An {@code EOObject} resenting the Java class instance
     */
    public EOObject EOnew(EOObject javaClass, EOObject a){
        EOObject result = new EOstring();
        try{
            Class<?> cls = Class.forName(javaClass._getData().toString());
            Constructor<?> constructor = cls.getConstructor(long.class);
            result = new EODataObject(constructor.newInstance(a._getData().toInt()).toString());
            return result;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }

    /**
     * @param javaClass an EO string object representing the specific java class to instantiate
     * @param methodName an EO string object representing the specific java method to execute
     * @param arguments EO string object(s) representing the arguments for the method to be executed
     * @return an {@code EOObject} representing the results of executing the java method
     */
    public EOObject EOinstanceMethod(EOObject javaClass, EOObject methodName, EOObject... arguments) {
        EOObject result = new EOstring();
        try {
            Class<?> cls = Class.forName(javaClass._getData().toString());
            Method method = Arrays.stream(cls.getMethods())
                    .filter(_method -> _method.getName().equals(
                            methodName._getData().toString())
                    ).findFirst().get();
            Parameter[] methodParams = method.getParameters();
            method.setAccessible(true);
            result = new EODataObject(
                    method.invoke(
                            cls.getConstructor().newInstance(),
                            _prepareArgs(methodParams, arguments)
                    ).toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return  result;
    }

    /**
     * Calls an instance method and turns it into an {@code EOObject}
     * @param javaClass an EO string object representing the specific java class whose method is to be executed
     * @param methodName an EO string object representing the specific java method to execute
     * @param arguments EO string object(s) representing the arguments to pass to the java method
     * @return an {@code EOObject} representing the results of executing the java method
     */
    public EOObject EOstatic(EOObject javaClass, EOObject methodName, EOObject... arguments) {
        EOObject result = new EOstring();
        try {
            Class<?> cls = Class.forName(javaClass._getData().toString());
            Method method = Arrays.stream(cls.getMethods())
                    .filter(_method -> _method.getName().equals(
                            methodName._getData().toString())
                    ).findFirst().get();
            Parameter[] methodParams = method.getParameters();
            method.setAccessible(true);
            result = new EODataObject(
                    method.invoke(
                            cls,
                            _prepareArgs(methodParams, arguments)
                    ).toString());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return  result;
    }

    private Object[] _prepareArgs(Parameter[] methodParams, EOObject... arguments) {
        List<Object> methodValues = new ArrayList<>();
        for (int i = 0; i < methodParams.length; i++) {
            if (methodParams[i].getType().getCanonicalName().endsWith("[]")) {
                methodValues.add(
                        Arrays
                        .stream(arguments)
                        .skip(i)
                        .toArray(EOObject[]::new));
                break;
            } else {
                methodValues.add(arguments[i]);
            }
        }
        return methodValues.toArray();
    }
}
