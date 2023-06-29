package org.eolang.parser;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import java.util.logging.Level;

public class StEoLogged implements Shift {

    private final Shift origin;
    private final Object target;
    private final Level level;

    public StEoLogged(final Shift origin) {
        this(origin, StEoLogged.class, Level.INFO);
    }

    public StEoLogged(final Shift origin, final Object target, final Level level) {
        this.origin = origin;
        this.target = target;
        this.level = level;
    }

    @Override
    public String uid() {
        return this.origin.uid();
    }

    @Override
    public XML apply(final int position, final XML xml) {
        final XML out;
        try {
            if (Logger.isEnabled(this.level, this.target)) {
                final String before = xml.toString();
                out = this.origin.apply(position, xml);
                final String after = out.toString();
                if (before.equals(after)) {
                    Logger.log(
                        this.level,
                        this.target,
                        "Shift #%d via '%s' made no changes",
                        position, this.uid()
                    );
                } else {
                    Logger.log(
                        this.level,
                        this.target,
                        "Shift #%d via '%s' produced (%d->%d chars):\n%s<EOF>",
                        position,
                        this.uid(),
                        before.length(),
                        after.length(),
                        after
                            .replace("\n", "\\n\n")
                            .replace("\t", "\\t\t")
                            .replace("\r", "\\r\r")
                    );
                }
            } else {
                out = this.origin.apply(position, xml);
            }
            // @checkstyle IllegalCatchCheck (1 line)
        } catch (final RuntimeException ex) {
            Logger.error(this.target, "The error happened here:%n%s", xml);
            throw new IllegalArgumentException(
                String.format("Shift '%s' failed", this.origin),
                ex
            );
        }
        return out;
    }
}
