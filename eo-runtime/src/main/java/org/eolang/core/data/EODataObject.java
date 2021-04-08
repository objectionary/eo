package org.eolang.core.data;

import org.eolang.core.EOObject;

/**
 * Объект хранящий данные, используется для работы с Data Objects из EO.
 */
public class EODataObject extends EOObject {

    /**
     * Хранимые данные.
     */
    protected EOData _data;

    public EODataObject(Object _data) {
        this._data = new EOData(_data);
    }

    public EODataObject() {
        this._data = new EONoData();
    }

    public EODataObject(EOData _data) {
        this._data = _data;
    }

    public boolean _isCalculable() {
        return true;
    }

    /**
     * Датаризация объекта.
     */
    public EOData _getData() {
        return _data;
    }

    public boolean isNoData() {
        return _data.getClass().equals(EONoData.class);
    }

    public EOObject _clone() {
        try {
            EOObject res = (EOObject) this.clone();
            return res;
        } catch (CloneNotSupportedException cnsException) {
            cnsException.printStackTrace();
        }
        return new EODataObject();
    }
}
