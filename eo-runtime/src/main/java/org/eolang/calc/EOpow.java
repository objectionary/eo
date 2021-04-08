package org.eolang.calc;

import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;
import org.eolang.core.data.EOMemoizedObject;

/**
 * Объект, при датаризации которого выполняется возведение в степеньЧЧ.
 */
public class EOpow extends EOMemoizedObject {
    private EOObject val1;
    private EOObject val2;

    public EOpow(EOObject val1, EOObject val2) {
        this.val1 = val1._setParent(this);
        this.val2 = val2._setParent(this);
        calculate();
    }

    private void calculate() {
        if (val1._isCalculable() && val2._isCalculable()) {
            if (val1._getData().isFloat() && val2._getData().isFloat()) {
                Double base = val1._getData().toFloat();
                Double exp = val2._getData().toFloat();
                Double result = Math.pow(base, exp);
                _data = new EOData(result);
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