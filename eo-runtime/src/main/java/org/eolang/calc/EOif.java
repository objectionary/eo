package org.eolang.calc;

import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;

/**
 * Объект представляющий собой аналог управляющей конструкции if в других языках.
 * Свободный атрибут cond(условие) определяет какой из объектов будет датаризован obj1 или obj2 для
 * вычисления результата датаризации этого объекта.
 */
public class EOif extends EOObject {
    private EOObject cond;
    private EOObject obj1;
    private EOObject obj2;

    public EOif(EOObject cond, EOObject obj1, EOObject obj2) {
        this.cond = cond._setParent(this);
        this.obj1 = obj1._setParent(this);
        this.obj2 = obj2._setParent(this);
    }

    @Override
    public EOData _getData() {
        Boolean boolCond = cond._getData().toBoolean();
        EOData res;
        if (boolCond) {
            res = obj1._getData();
        } else {
            res = obj2._getData();
        }
        //_freeAttributes();
        return res;
    }
}
