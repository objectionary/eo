package org.eolang.java;

import org.eolang.EOstring;
import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;
import java.util.Date;

public class EOutil extends EOObject {
    public EOstring EOdate(){
        return new EOstring(new Date().toString());
    }

    @Override
    public EOData _getData() {
        return super._getData();
    }
}
