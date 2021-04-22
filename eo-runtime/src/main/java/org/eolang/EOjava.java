package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

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

    @Override
    public EOData _getData() {
        return super._getData();
    }
}
