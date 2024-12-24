package org.eolang.parser.errors;

import java.util.Iterator;
import org.antlr.v4.runtime.BaseErrorListener;
import org.xembly.Directive;

public final class EoParserErrors extends BaseErrorListener implements Iterable<Directive> {
    @Override
    public Iterator<Directive> iterator() {
        return null;
    }
}
