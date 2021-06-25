package org.eolang.hse.core;

import org.eolang.hse.EOstring;
import org.eolang.hse.core.data.EODataObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Parameter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Main {
    private final PrintStream stdout;

    /**
     * Constructor
     *
     * @param out The output
     */
    public Main(final PrintStream out) {
        this.stdout = out;
    }

    /**
     * The method called by JVM when the program starts.
     *
     * @param args Command line args
     * @throws Exception If fails
     */
    public static void main(final String... args) throws Exception {
        new Main(System.out).exec(args);
    }

    /**
     * The same method, but not static.
     *
     * @param args Command line args
     * @throws Exception If fails
     */
    public void exec(final String... args) throws Exception {
        if (args.length == 0 || "--version".equals(args[0])) {
            this.version();
            return;
        }
        final String path = args[0].replaceAll("([^.]+)$", "EO$1");
        Constructor<?> appConstructor = Arrays.stream(Class.forName(path).getConstructors())
                .findFirst().get();
        Parameter[] appParams = appConstructor.getParameters();
        List<Object> appValues = new ArrayList<>();
        for (int i = 0; i < appParams.length; i++) {
            if (appParams[i].getType().getCanonicalName().endsWith("[]")) {
                List<EOstring> objs = Arrays.stream(args).skip(i + 1).map(arg -> new EOstring(arg)).collect(Collectors.toList());
                appValues.add(objs.toArray(new EOstring[0]));
                break;
            } else {
                Object obj = new EODataObject(args[i + 1]);
                appValues.add(obj);
            }
        }
        EOObject app = (EOObject) appConstructor.newInstance(appValues.toArray());
        System.out.println(String.format("The program has dataized to: %s", app._getData().toString()));
    }

    /**
     * Reads the version from resources and prints it.
     *
     * @throws IOException If fails
     */
    private void version() throws IOException {
        try (BufferedReader input =
                     new BufferedReader(
                             new InputStreamReader(//${project.version}
                                     Objects.requireNonNull(Main.class.getResourceAsStream("version.txt")),
                                     StandardCharsets.UTF_8)
                     )
        ) {
            if (input.lines().findFirst().isPresent()) {
                this.stdout.printf(
                        "Eolang Runtime v.%s",
                        input.lines().findFirst().get()
                );
            }
        }
    }
}
