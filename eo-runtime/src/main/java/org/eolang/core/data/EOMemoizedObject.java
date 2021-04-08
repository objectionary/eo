package org.eolang.core.data;

import org.eolang.core.EOObject;

/**
 * Базовый класс для объектов представляющие арифметические, логические и другие операции над данными.
 * Объекты этого типа имеют возможность произвести датаризацию при создании и кешировать результат.
 */
public abstract class EOMemoizedObject extends EOObject {
    /**
     * Кешированные данные
     */
    protected EOData _data;

    public boolean _isCalculable() {
        return _data != null;
    }
}
