#!/usr/bin/perl -w
#
# Based on chronicle's comments.cgi.
#
#  This is a simple script which is designed to accept comment requests,
# and save the details to local text files upon the localhost.
#
# This code is very simple and should be easy to extend with anti-spam
# at a later point.
#
#   NOTE:  If you wish to use this you must edit three things at the
#         top of the script.
#
#          1.  The email address to notify.
#          2.  The email address to use as the sender.

use strict;
use warnings;

use CGI;
use POSIX qw(strftime);

#
#  The notification addresses - leave blank to disable
#
my $TO   = 'comments@hugoduncan.org';
my $FROM = 'weblog@hugoduncan.org';

#
#  Find sendmail
#
my $SENDMAIL = undef;
foreach my $file (qw ! /usr/lib/sendmail /usr/sbin/sendmail !)
{
    $SENDMAIL = $file if ( -x $file );
}


#
#  Get the parameters from the request.
#
my $cgi  = new CGI();
my $name = $cgi->param('name') || undef;
my $mail = $cgi->param('mail') || undef;
my $body = $cgi->param('body') || undef;
my $id   = $cgi->param('id') || undef;
my $uri  = $cgi->param('uri') || undef;
my $cap  = $cgi->param('captcha') || undef;
my $ajax = $cgi->param("ajax") || 0;


#
#  If any are missing just redirect back to the referer.
#
if ( !defined($name) ||
     !length($name) ||
     !defined($uri) ||
     !length($uri) ||
     !defined($body) ||
     !length($body) ||
     !defined($id) ||
     !length($id) )
{
    if ($ajax)
    {
        print "Content-type: text/html\n\n";
        print "Missing fields.\n";
    }
    else
    {
        print "Location: " . $ENV{ 'HTTP_REFERER' } . "\n\n";
    }
    exit;
}

#
#  Does the captcha value contain text?  If so spam.
#
if ( defined($cap) && length($cap) )
{
    if ($ajax)
    {
        print "Content-type: text/html\n\n";
        print "Missing fields.\n";
    }
    else
    {
        print "Location: " . $ENV{ 'HTTP_REFERER' } . "/\n\n";
    }
    exit;
}



#
# get the current time
#
my $timestr = strftime "%Y-%m-%dT%H:%M:%S.000000Z", gmtime;


#
#  Send a mail.
#
if ( length($TO) && length($FROM) && defined($SENDMAIL) )
{
    open( SENDMAIL, "|$SENDMAIL -t -f $FROM" );
    print SENDMAIL "To: $TO\n";
    print SENDMAIL "From: $FROM\n";
    print SENDMAIL "Subject: New Comment [$id]\n";
    print SENDMAIL "Content-type: text/html\n";
    print SENDMAIL "X-Blogen-Name: $name\n";
    print SENDMAIL "X-Blogen-Mail: $mail\n";
    print SENDMAIL "X-Blogen-Uri: $uri\n";
    print SENDMAIL "X-Blogen-User-Agent: $ENV{'HTTP_USER_AGENT'}\n";
    print SENDMAIL "X-Blogen-IP-Address: $ENV{'REMOTE_ADDR'}\n";
    print SENDMAIL "X-Blogen-When: $timestr\n";
    print SENDMAIL "X-Blogen-id: $id\n";
    print SENDMAIL "\n";
    print SENDMAIL $body;
    close(SENDMAIL);
}

#
#  Now show the user the thanks message..
#

#
#  Show the header
#
print "Content-type: text/html\n\n";

if ( $cgi->param("ajax") )
{
    print <<EOF;
<h3>Comment Submitted</h3>
<blockquote>
<p>Thanks for your comment, it will be made live when the queue is moderated next.</p>
</blockquote>

EOF
    exit;
}
else
{
    print <<EOF;
<html>
 <head>
  <title>Thanks For Your Comment</title>
 </head>
 <body>
  <h2>Thanks!</h2>
  <p>Your comment will be included the next time the comments are moderated.</p>
  <p><a href="$ENV{'HTTP_REFERER'}">Return to blog</a>.</p>
 </body>
</html>
EOF
}
