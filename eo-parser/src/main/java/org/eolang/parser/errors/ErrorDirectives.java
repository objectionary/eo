package org.eolang.parser.errors;

import java.util.Iterator;
import java.util.List;
import org.cactoos.iterable.Mapped;
import org.eolang.parser.ParsingException;
import org.xembly.Directive;
import org.xembly.Directives;

public final class ErrorDirectives implements Iterable<Directive> {

    /**
     * Errors accumulated.
     */
    private final List<ParsingException> errors;

    /**
     * Ctor.
     * @param errors The errors.
     */
    public ErrorDirectives(final List<ParsingException> errors) {
        this.errors = errors;
    }

    @Override
    public Iterator<Directive> iterator() {
        return new org.cactoos.iterable.Joined<>(
            new Mapped<Iterable<Directive>>(
                error -> new Directives()
                    .xpath("/program")
                    .strict(1)
                    .addIf("errors")
                    .strict(1)
                    .add("error")
                    .attr("check", "eo-parser")
                    .attr("line", error.line())
                    .attr("severity", "critical")
                    .set(error.getMessage()),
                this.errors
            )
        ).iterator();
    }
}
