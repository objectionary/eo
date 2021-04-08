package org.eolang.calc;

import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;
import org.eolang.core.data.EOMemoizedObject;

/**
 * Объект, при датаризации которого выполняется арифметическое умножение результатов датаризации свободных атрибутов.
 */
public class EOmul extends EOMemoizedObject {
    private EOObject val1;
    private EOObject val2;

    public EOmul(EOObject val1, EOObject val2) {
        this.val1 = val1._setParent(this);
        this.val2 = val2._setParent(this);
        calculate();
    }

    private void calculate() {
        if (val1._isCalculable() && val2._isCalculable()) {
            if (val1._getData().isInteger() && val2._getData().isInteger()) {
                _data = new EOData(val1._getData().toInt() * val2._getData().toInt());
            } else if (val1._getData().isFloat() && val2._getData().isFloat()) {
                _data = new EOData(val1._getData().toFloat() * val2._getData().toFloat());
            }
        }
    }

    @Override
    public EOData _getData() {
        if (!_isCalculable()) {
            calculate();
        }
        return _data;
    }
}