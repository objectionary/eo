package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class EOjava extends EOObject {

    public EOObject EOnew(EOObject javaClass){
        EOstring result = new EOstring();
        try{
            Class<?> cls = Class.forName(javaClass._getData().toString());
            Constructor<?> constructor = cls.getConstructor();
            System.out.println("Print instance: " + constructor.newInstance());
            result = new EOstring((String) constructor.newInstance());
            return result;
        } catch (ClassNotFoundException | NoSuchMethodException | InvocationTargetException | IllegalAccessException | InstantiationException e) {
            e.printStackTrace();
        }
        return result;
    }

    public EOObject EOget(EOObject javaClass, EOstring methodName){
        EOstring result = new EOstring();
        System.out.println("Trying...");
        try{
            Class<?> cls = Class.forName(javaClass._getData().toString());
            Method method = cls.getDeclaredMethod(methodName._getData().toString());
            method.setAccessible(true);
            System.out.println("Print method: " + method.invoke(cls));
            result = new EOstring((String) method.invoke(cls));
            return result;
        } catch (ClassNotFoundException | NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
            e.printStackTrace();
        }
        return result;
    }

    @Override
    public EOData _getData() {
        return super._getData();
    }
}
