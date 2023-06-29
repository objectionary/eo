package org.eolang.parser;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import java.util.function.Consumer;

final class StEoLogged implements Shift {

    private final Shift origin;
    private final Consumer<? super String> logger;

    StEoLogged(final Shift shift) {
        this(shift, message -> Logger.error(StEoLogged.class, message));
    }

    StEoLogged(final Shift origin, final Consumer<? super String> logger) {
        this.origin = origin;
        this.logger = logger;
    }

    @Override
    public String uid() {
        return this.origin.uid();
    }

    @Override
    public XML apply(final int position, final XML xml) {
        try {
            return this.origin.apply(position, xml);
            // @checkstyle IllegalCatchCheck (1 line)
        } catch (final RuntimeException ex) {
            this.logger.accept(
                String.format(
                    "Eo representation of the parsed xml: %n%s",
                    new XMIR(xml).toEO()
                )
            );
            throw new IllegalStateException(
                String.format("Shift '%s' failed", this.origin),
                ex
            );
        }
    }
}
