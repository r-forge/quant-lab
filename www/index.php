
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.1" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<table summary="Package quantlab2 summary">
  <tbody>
		<tr>
			<td valign="top">
				Version:</td>
			<td>
				1.1</td>
		</tr>
		<tr>
			<td valign="top">
				Depends:</td>
			<td>
				<a href="http://cran.r-project.org/web/packages/fOptions/index.html">fOptions</a></td>
    </tr>
		<tr>
			<td valign="top">
				Published:</td>
			<td>
				2014-11-30</td>
		</tr>
		<tr>
			<td valign="top">
				Author:</td>
			<td>
				Oleg Mubarakshin</td>
		</tr>
		<tr>
			<td valign="top">
				Maintainer:</td>
			<td>
				Oleg Mubarakshin &lt;oleg.mubarakshin at gmail.com&gt;</td>
		</tr>
		<tr>
			<td valign="top">
				License:</td>
			<td>
        <a href="http://cran.r-project.org/web/licenses/GPL-2">GPL-2</a> | <a href="http://cran.r-project.org/web/licenses/GPL-3">GPL-3</a> [expanded from: GPL (≥ 2)]</td>
		</tr>
		<tr>
			<td valign="top">
				URL:</td>
			<td>
				<a href="http://quant-lab.com/r.html">http://quant-lab.com/R/</a></td>
		</tr>
	</tbody>
</table>

<h4 style="background-color: rgb(255, 255, 255); color: rgb(102, 102, 102); font-family: monospace;">
	Downloads:</h4>

<table summary="Package quantlab2 downloads">
	<tbody>
		<tr>
			<td valign="top">
				Reference&nbsp;manual:</td>
			<td>
				Currently not available</td>
		</tr>
		<tr>
			<td valign="top">
				Package&nbsp;source:</td>
			<td>
        <a href="http://download.r-forge.r-project.org/src/contrib/quantlab2_1.0.tar.gz"> quantlab2_1.0.tar.gz </a></td>
		</tr>
		<tr>
			<td valign="top">
				MacOS&nbsp;X&nbsp;binary:</td>
			<td>
				Currently not available</td>
		</tr>
		<tr>
			<td valign="top">
				Windows&nbsp;binary:</td>
			<td>
				<a href="http://download.r-forge.r-project.org/bin/windows/contrib/latest/quantlab2_1.0.zip"> quantlab2_1.0.zip </a></td>
		</tr>
	</tbody>
</table>

<h4 style="background-color: rgb(255, 255, 255); color: rgb(102, 102, 102); font-family: monospace;">
	Install:</h4>

<table summary="Package quantlab2 downloads">
	<tbody>
		<tr>
			<td valign="top">
				R&nbsp;install&nbsp;command:</td>
			<td>
				install.packages("quantlab2", repos="http://R-Forge.R-project.org")</td>
		</tr>
	</tbody>
</table>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
