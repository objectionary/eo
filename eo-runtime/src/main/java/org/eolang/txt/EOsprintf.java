package org.eolang.txt;

import org.eolang.EOarray;
import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;

import java.util.Arrays;
import java.util.Objects;

/**
 * Объект осуществляющий создание форматированых строк.
 */
public class EOsprintf extends EOObject {
    private EOObject format;
    private EOObject[] data;

    public EOsprintf(EOObject format, EOObject... data) {
        this.format = format;
        this.data = data;
    }

    @Override
    public EOData _getData() {
        String sFormat = format._getData().toString();
        Object[] objects = Arrays.stream(data).map(obj -> obj._getData().toObject()).toArray();
        return new EOData(
                String.format(
                        sFormat,
                        objects
                )
        );
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || !(o instanceof EOObject)) return false;
        EOObject eoObject = (EOObject) o;
        return _getData().toString().equals(eoObject._getData().toString());
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(format);
        result = 31 * result + Arrays.hashCode(data);
        return result;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("sprintf(");
        sb.append("format:").append(format);
        sb.append(", data:");
        sb.append(new EOarray(data).toString());
        sb.append(')');
        return sb.toString();
    }
}
