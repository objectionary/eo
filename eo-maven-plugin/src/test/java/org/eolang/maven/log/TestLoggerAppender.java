package org.eolang.maven.log;

import java.util.Enumeration;
import org.apache.log4j.Appender;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;

public class TestLoggerAppender extends ConsoleAppender {

    private final Logs logs;

    TestLoggerAppender(final Logs logs) {
        this.logs = logs;
    }

    @Override
    public void append(final LoggingEvent event) {
        this.logs.append(this.getLayout().format(event));
    }

    void init() {
        final Logger logger = LogManager.getRootLogger();
        final Enumeration<?> appenders = logger.getAllAppenders();
        if (appenders.hasMoreElements()) {
            final Object next = appenders.nextElement();
            if (next instanceof ConsoleAppender) {
                this.setLayout(((Appender) next).getLayout());
            }
        }
        logger.addAppender(this);
    }

    void remove() {
        LogManager.getRootLogger().removeAppender(this);
    }
}
