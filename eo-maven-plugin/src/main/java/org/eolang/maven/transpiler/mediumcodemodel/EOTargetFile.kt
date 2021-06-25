package org.eolang.maven.transpiler.mediumcodemodel

/***
 * Represents an output Java file with a name and contents
 * @param fileName the name of the output Java file
 * @param contents the contents of the output Java file
 */
class EOTargetFile(val fileName: String, val contents: String)