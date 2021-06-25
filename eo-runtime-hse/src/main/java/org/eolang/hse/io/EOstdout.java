package org.eolang.hse.io;

import org.eolang.hse.core.EOObject;
import org.eolang.hse.core.data.EOData;

/**
 * Объект при датаризации выводит в стандартный поток вывода результат датаризации свободного атрибута out.
 * Результат датаризации объекта - результат датаризации свободного атрибута out.
 */
public class EOstdout extends EOObject {

    private EOObject out;

    /**
     * Instantiates a new stdout.
     *
     * @param out the out
     */
    public EOstdout(EOObject out) {
        this.out = out;
    }

    @Override
    public EOData _getData() {
        EOData res = out._getData();
        System.out.print(res);
        return res;
    }
}
