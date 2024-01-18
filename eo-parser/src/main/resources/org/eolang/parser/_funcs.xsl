<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2024 Objectionary.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="_funcs" version="2.0">
  <xsl:function name="eo:abstract" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="not(exists($o/@base)) and (exists($o/o) or $o/@atom or $o/@abstract)"/>
  </xsl:function>
  <xsl:function name="eo:attr" as="xs:boolean">
    <xsl:param name="o" as="element()"/>
    <xsl:sequence select="$o/parent::o[not(@base)] and not($o/@base) and not($o/@atom) and not($o/o)"/>
  </xsl:function>
  <xsl:function name="eo:alias-name" as="xs:string">
    <xsl:param name="object" as="element()"/>
    <xsl:sequence select="tokenize($object/tail, ' ')[1]"/>
  </xsl:function>
  <xsl:function name="eo:alias-qualified" as="xs:string">
    <xsl:param name="object" as="element()"/>
    <xsl:sequence select="tokenize($object/tail, ' ')[2]"/>
  </xsl:function>
  <xsl:function name="eo:bytes-to-int" as="xs:integer">
    <xsl:param name="bytes"/>
    <xsl:sequence select="if (string-length($bytes) = 0) then 0 else string-length(substring-before('0123456789ABCDEF', substring($bytes, string-length($bytes), 1))) + 16 * eo:bytes-to-int(substring($bytes, 1, string-length($bytes) - 1))"/>
  </xsl:function>
</xsl:stylesheet>
