package eo;

import java.util.Arrays;
import java.util.List;

public final class App implements Runnable {

    private final cli cli;

    private App(final List<String> args) {
        this.cli = new cli(args);
    }

    @Override
    public void run() {
        this.cli.run();
    }

    public static void main(final String[] args) {
        final Thread thread = new Thread(new App(Arrays.asList(args)));
        thread.start();
        try {
            thread.join();
        } catch (final InterruptedException ex) {
            System.out.println(ex.getLocalizedMessage());
        }
    }
}