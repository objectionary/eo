<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:saxon="http://saxon.sf.net/"
		xmlns:xs="http://www.w3.org/2001/XMLSchema"
		xpath-default-namespace="http://gandhimukul.tripod.com/xslt/ruleset"
		exclude-result-prefixes="saxon xs"
                version="2.0">

   <xsl:output method="xml" indent="yes" omit-xml-declaration="yes" />

   <xsl:param name="ruleset" />
   <xsl:param name="folder" select="''" />

   <!-- default target is 'xml' (parameter might be absent) -->
   <xsl:param name="target" select="''" />

   <xsl:variable name="rules" select="document($ruleset)" />

   <xsl:template match="/">
     <xsl:choose>
       
       <!-- process a single file -->
       <xsl:when test="$folder = ''">
         <xsl:variable name="input" select="." />
         <xsl:choose>
           <xsl:when test="$target = 'xml' or $target = ''">
	     <violations>
               <xsl:call-template name="generate-report-content">
                 <xsl:with-param name="input" select="." />
	       </xsl:call-template>
             </violations>
           </xsl:when>
           <xsl:when test="$target = 'html'">
             <html>
	       <head>
	         <title>Violation Report</title>
	       </head>
	       <body>
                 <table border="1">
		   <tr>
                    <th>File</th>
		    <th>Rule</th>
		    <th>Priority</th>
		    <th>Line</th>
		    <th>Message</th>
		   </tr>
                   <xsl:call-template name="generate-report-content">
                     <xsl:with-param name="input" select="." />
	           </xsl:call-template>
	        </table>
	      </body>
	    </html>
          </xsl:when>
          <xsl:when test="$target = 'csv'">
            <xsl:text>File,Rule,Priority,Line,Message&#xa;</xsl:text>
	    <xsl:call-template name="generate-report-content">
              <xsl:with-param name="input" select="." />
	    </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            The report target &apos;<xsl:value-of select="$target" />&apos; is not supported
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>

      <xsl:otherwise>
        <!-- process a folder location -->
	<xsl:choose>
	  <xsl:when test="(($target = 'xml') or ($target = '') or ($target = 'html') or ($target = 'csv'))">
	    <xsl:variable name="file-collection" select="collection(concat($folder,'?select=*.xsl;recurse=yes'))" />
	    <xsl:choose>
	      <xsl:when test="$target = 'xml' or $target = ''">
	        <violations>
	          <xsl:for-each select="$file-collection">
                    <xsl:call-template name="generate-report-content">
                      <xsl:with-param name="input" select="." />
	            </xsl:call-template>
	          </xsl:for-each>
		</violations>
              </xsl:when>
	      <xsl:when test="$target = 'html'">
                <html>
	         <head>
	           <title>Violation Report</title>
	         </head>
	         <body>
                   <table border="1">
		     <tr>
                       <th>File</th>
		       <th>Rule</th>
		       <th>Priority</th>
		       <th>Line</th>
		       <th>Message</th>
		     </tr>
		     <xsl:for-each select="$file-collection">
                       <xsl:call-template name="generate-report-content">
                         <xsl:with-param name="input" select="." />
	               </xsl:call-template>
	             </xsl:for-each>
                   </table>
                 </body>
               </html> 
	      </xsl:when>
	      <xsl:when test="$target = 'csv'">
                <xsl:text>File,Rule,Priority,Line,Message&#xa;</xsl:text>
                <xsl:for-each select="$file-collection">
                  <xsl:call-template name="generate-report-content">
                    <xsl:with-param name="input" select="." />
                  </xsl:call-template>
                </xsl:for-each>
	      </xsl:when>
	    </xsl:choose>
          </xsl:when>
	  <xsl:otherwise>
            The report target &apos;<xsl:value-of select="$target" />&apos; is not supported
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>

    </xsl:choose>
   </xsl:template>

   <xsl:template name="generate-report-content">
     <xsl:param name="input" />
     
     <xsl:for-each select="$rules/ruleset/rule">
       <xsl:variable name="rule-name" select="@name" />
       <xsl:variable name="priority" select="priority" />
       <xsl:variable name="message" select="normalize-space(message)" />
       <xsl:variable name="xpath" select="normalize-space(xpath)" />
       <xsl:variable name="xpath-eval" select="$input/saxon:evaluate($xpath)" />
       <xsl:for-each select="$xpath-eval">
         <xsl:choose>
           <xsl:when test="$target = 'xml' or $target = ''">
             <violation>
               <file><xsl:value-of select="document-uri($input)" /></file>
               <rule><xsl:value-of select="$rule-name" /></rule>
	       <priority><xsl:value-of select="$priority" /></priority>
	       <line><xsl:value-of select="saxon:line-number()" /></line>
	       <message><xsl:value-of select="$message" /></message>
   	     </violation>
           </xsl:when>
           <xsl:when test="$target = 'html'">
              <tr>
		<td><xsl:value-of select="document-uri($input)" /></td>	
		<td><xsl:value-of select="$rule-name" /></td>
		<td><xsl:value-of select="$priority" /></td>
		<td><xsl:value-of select="saxon:line-number()" /></td>
		<td><xsl:value-of select="$message" /></td>
	      </tr>
           </xsl:when>
           <xsl:when test="$target = 'csv'">
             <xsl:value-of select="document-uri($input)" />,<xsl:value-of select="$rule-name" />,<xsl:value-of select="$priority" />,<xsl:value-of select="saxon:line-number()" />,<xsl:value-of select="$message" /><xsl:text>&#xa;</xsl:text>
           </xsl:when>
         </xsl:choose>
       </xsl:for-each>
     </xsl:for-each>
   </xsl:template>

</xsl:stylesheet>