
################################################
#function to send email using Outlook
################################################
outlookSend <- function(To, Subject='', CC=NULL, BCC=NULL,
                        HTMLBodyFile=NULL, HTMLBody=NULL, Body='', Attachments=NULL,
                        DeferredDeliveryTime=NULL, OriginatorDeliveryReportRequested=NULL, ReadReceiptRequested=NULL,
                        FlagRequest=NULL, Importance=NULL, Sensitivity=NULL,
                        SentOnBehalfOfName=NULL)
{

  ################################################
  #Outlook Enumerations
  ################################################
  #OlMailRecipientType Enumeration (Outlook)
  OlMailRecipientType  <- c(olBCC=3, olCC=2, olOriginator=0, olTo=1)

  #OlBodyFormat Enumeration (Outlook)
  OlBodyFormat <- c(olFormatHTML=2, olFormatPlain=1, olFormatRichText=3, olFormatUnspecified=0)

  #OlImportance Enumeration (Outlook)
  OlImportance <- c(olImportanceHigh=2, olImportanceLow=0, olImportanceNormal=1)

  #OlSensitivity Enumeration (Outlook)
  OlSensitivity <- c(olConfidential=3, olNormal=0, olPersonal=1, olPrivate=2)

  #OlDefaultFolders Enumeration (Outlook)
  OlDefaultFolders <- c(olFolderCalendar=9, olFolderConflicts=19, olFolderContacts=10, olFolderDeletedItems=3,
                        olFolderDrafts=16, olFolderInbox=6, olFolderJournal=11, olFolderJunk=23,
                        olFolderLocalFailures=21, olFolderManagedEmail=29, olFolderNotes=12, olFolderOutbox=4,
                        olFolderSentMail=5, olFolderServerFailures=22, olFolderSyncIssues=20,
                        olFolderTasks=13, olFolderToDo=28, olPublicFoldersAllPublicFolders=18, olFolderRssFeeds=25)


  vbscript <- c('Dim objOutlook',
                'Dim objMailItem',
                'Dim objFileSystem',
                'Dim objNamespace',
                'Dim objSentFolder',

                'Set objOutlook = CreateObject("Outlook.Application")',
                'Set objMailItem = objOutlook.CreateItem(0)',

                'With objMailItem',
                paste0('\t.Subject = "', Subject, '"'),

                #Add recipients
                unlist(lapply(To, function(recip) c(
                  paste0('\tSet recip = .Recipients.Add ("',recip,'")'),
                  paste('\trecip.Type =',OlMailRecipientType['olTo'])))),

                if (!is.null(CC)) {
                  unlist(lapply(To, function(recip) c(
                    paste0('\tSet recip = .Recipients.Add ("',recip,'")'),
                    paste('\trecip.Type =',OlMailRecipientType['olCC']))))
                },
                if (!is.null(BCC)) {
                  unlist(lapply(To, function(recip) c(
                    paste0('\tSet recip = .Recipients.Add ("',recip,'")'),
                    paste('\trecip.Type =',OlMailRecipientType['olBCC']))))
                },
                '\t.Recipients.ResolveAll',

                #Insert email body
                if (!is.null(HTMLBodyFile)) {
                  c(paste('\t.BodyFormat =', OlBodyFormat['olFormatHTML']),
                    '\tSet objFileSystem = CreateObject("Scripting.FileSystemObject")',
                    paste0('\tSet file = objFileSystem.OpenTextFile("',HTMLBodyFile,'", 1)'),
                    '\t.HTMLBody = file.ReadAll')

                } else if (!is.null(HTMLBody)) {
                  c(paste('\t.BodyFormat =', OlBodyFormat['olFormatHTML']),
                    paste0('\t.HTMLBody = "',HTMLBody,'"'))

                } else {
                  c(paste('\t.BodyFormat =', OlBodyFormat['olFormatPlain']),
                    paste0('\t.Body = "', Body, '"'))

                },

                #Add attachments
                if (!is.null(Attachments)) {
                  vapply(Attachments, function(x) paste0('\t.Attachments.Add  "',x,'"'), character(1))
                },

                #copy of the mail message is saved down
                '\t.DeleteAfterSubmit = False',

                #Delay delivery in minutes
                if (!is.null(DeferredDeliveryTime)) {
                  paste0('\t.DeferredDeliveryTime = dateadd("n", ',DeferredDeliveryTime,', Now)')
                },

                #Indicates the requested action for a mail item
                if (!is.null(FlagRequest)) {
                  '\t.FlagRequest = "Follow up"'
                },

                #Indicates the relative importance level for the mail item
                if (!is.null(Importance)) {
                  paste('\t.Importance =', OlImportance[Importance])
                },

                #OriginatorDeliveryReportRequested of type logical: whether to receive delivery report
                if (!is.null(OriginatorDeliveryReportRequested) && OriginatorDeliveryReportRequested) {
                  '\t.OriginatorDeliveryReportRequested = True'
                },

                #ReadReceiptRequested of type logical: request read receipt
                if (!is.null(ReadReceiptRequested) && ReadReceiptRequested) {
                  '\t.ReadReceiptRequested = True'
                },

                #Indicates the the display name for the intended sender of the mail message
                if (!is.null(SentOnBehalfOfName)) {
                  paste('\t.SentOnBehalfOfName =', SentOnBehalfOfName)
                },

                #Indicates the sensitivity for the Outlook item
                if (!is.null(Sensitivity)) {
                  paste('\t.Sensitivity =', OlSensitivity[Sensitivity])
                },

                'End With',
                'objMailItem.Send',
                'Set objFileSystem = Nothing',
                'Set objMailItem = Nothing',
                'Set objOutlook = Nothing')

  vbsfile <- tempfile('vbs',getwd(),'.vbs')
  vbscript
  write(vbscript, vbsfile)
  shell(paste('cscript /NOLOGO', vbsfile))
  unlink(vbsfile)
  vbscript
} #outlookSend

