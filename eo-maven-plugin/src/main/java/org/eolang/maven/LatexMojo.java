package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public final class LatexMojo extends SafeMojo{
    public static final String latex = "eo-runtime/target/eo/latex/";
    public static final String DIR = "eo-runtime/target/eo/03-optimize/org/eolang";
    int counter = 1;
    void rec(File[] files, String subdir) throws IOException {
        for(File child: files){
            if(child.isDirectory()) {
                new File(latex + subdir + child.getName()).mkdirs();
                rec(child.listFiles(), subdir + child.getName() + "/");
                continue;
            }
            String name = child.getName();
            String fileNameWithoutExtension = name.substring(0, name.lastIndexOf('.'));
            String put = latex + subdir + fileNameWithoutExtension + ".tex";
            new File(put).createNewFile();
        }
    }
    @Override
    void exec() throws IOException {
        if(!Files.exists(Paths.get(latex))) {
            new File(latex).mkdirs();
            new File(latex + "universe.tex").createNewFile();
        }
        if(Files.exists(Paths.get(DIR))) {
            System.out.println("We run it " + counter + " times.");
            counter++;
            System.out.println();
            File[] files = new File(DIR).listFiles();
            rec(files,"");
        }

    }
}
