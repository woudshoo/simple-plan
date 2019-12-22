@*Introduction.  This is a project planning program with a focus on rough cut planning.
Features that are different from the typical project planning applications are:
\unorderedlist
\li Automated planning
\li Allow alternative production rules
\li Allow specification of when production rules are allowed
\endunorderedlist

The automated planning is achieved by translating the planning problem
into a Mixed Integer Linear Programming (MILP) problem.  This is where
the {\it rough cut\/} part comes into play.  In order to keep the
problem small enough, time is broken up in buckets.  For example one
bucket per week.  The MILP problem will not see details inside a
bucket and dependencies are only observed between buckets.

@ The program consists of one package |simple-plan|.

@l
@e
(eval-when (:compile-toplevel :load-toplevel :execute)
   @<Required Packages@>)

@e
(defpackage "SIMPLE-PLAN"
   (:documentation "Simple planning, a rough cut project planning tool.")
   (:use "COMMON-LISP")
   @<Package Imports@>)

@e
(in-package "SIMPLE-PLAN")

@ The general order of the code in the source file follows this pattern

@l
@<API Definitions@>
@<Base Classes@>

@*Moments and Events.  Project planning is based on time, and for planning there are two aspects of time important
\unorderedlist
\li A moment.  A specific moment in time.  This has no duration, but
    is used to identify the start, end or deadline of a task.
    Examples are {\it 2019-09-23 18:34 GMT}.  These are the focus of
    this section.
\li A time span.  A slice of time with a begin and end.  This has a
    duration. 
\endunorderedlist

Moments are anonymous, they are purely identified by the moment in
time.  Events are special moments, they have a name and can be
identified by more than just the moment in time.

@ For the presentation of moments we use universal time.  Which
imports the restrictions that we cannot represent time before the year
1900 and that the resolution is one second.  However, the choice of
time is limited to the |moment| class and hidden behind an interface.  So
we can later replace it, if needed, with another time representation.

@l 
(defclass moment ()
  ((timestamp)))

@ In addition to normal timestamps, there are two special timestamps.
They are $-\infty$ an $+\infty$.

@l
(defvar +moment-before-time+ (make-instance 'moment))
(defvar +moment-after-time+  (make-instance 'moment))

@ These special moments can be identifed with a simple method

@l
(defgeneric special-p (object)
  (:method ((object t)) nil)
  (:method ((object (eql +moment-before-time+))) t)
  (:method ((object (eql +moment-after-time+))) t))

@ Between two moments there passes a certain amount of time.  This is
determined by the |time-between| method.  If the first argument is
after the second argument the number will be negative.  If the
duration is infinite, the returned value is nil.

@l
(defgeneric time-between (first-moment second-moment)
  (:method ((first-moment moment) (second-moment moment))
    (if (or (special-p first-moment)
            (special-p second-moment))
       nil
       (- (slot-value second-moment 'timestamp)
          (slot-value first-moment 'timestamp)))))

@ Obviously timestamps can also be ordered.  The primary method is
|before| which indicates if the first argument happens before the
second.

@l

(defgeneric before (first-moment second-moment)
  (:method ((first-moment moment) (second-moment moment))
    (cond
       ((eq first-moment second-moment)
          nil)
       ((or (eq first-moment +moment-before-time+)
            (eq second-moment +moment-after-time+))
          t)
       ((or (eq first-moment +moment-after-time+)
            (eq second-moment +moment-before-time+))
          nil)  
       ((> (time-between first-moment second-moment) 0)
          t)
       (t nil))))

@ And the reverse comparison is implemented in terms of the first

@l
(defgeneric after (first-moment second-moment)
  (:method ((first-moment moment) (second-moment moment))
    (before second-moment first-moment)))


@ Before we delve into how to create moments and convert them to and from a string,
And an event is a moment, but has a name and an identitity.  

@l
(defclass event (moment identifiable)
  ()
  (:default-initargs
    :category 'event))

@2* Timestamp Specifications.  Timestamps are parsed by the |LOCAL-TIME| package.

The external representation of |moment| is handled by the package |LOCAL-TIME|

@<Required Packages@>=
   (require "LOCAL-TIME")

@ @<Package Imports@>=
   (:import-from "LOCAL-TIME" "PARSE-TIMESTRING")

@2* Time Spans. Time spans represent a half open range of time $[b,
e)$ with $b$ and $e$ moments.  They conceptually form the basis for
the planning buckets and calendars.

@l
(defclass time-span ()
  ((begin :type moment :reader begin :initform +moment-before-time+)
   (end   :type moment :reader end   :initform +moment-after-time+)))

@ Also we will use identifiable time-span object in the construction of Calendars and Buckets.

@l
(defclass named-time-span (time-span identifiable)  ())

@ The time spans that are infinitely long are special

@l
(defmethod special-p ((time-span time-span))
  (or (special-p (begin time-span))
      (special-p (end time-span))))


@ In addition, the time span that covers all of time is used a bit and therefore gets its own name.

@l
(defvar +time-span-all+ (make-instance 'time-span))

@ They time spans have have also a duration

@l
(defgeneric duration (time-span)
  (:method ((time-span time-span))
     (time-between (begin time-span) (end time-span))))



@*Calendars.  Calendars are central to the simple-plan system.
Calendars control when a product can be made, when a ingredient is
available, control when people are available etc.  In addition they
determine the efficiency of production and can be used as an
indication of cost/benefit.

For this Calendars are implemented as a piecewise constant function
$C$ over time.  To be a bit more precise, the time range
$(-\infty,\infty)$ is divided up by a countable collection ${\cal I}$
of half open intervals.  That is, each interval $I\in {\cal I}$ is of
the form $I=[s_I, e_I)$. Also $(-\infty,\infty) = \cup_{I\in {\cal I}} I$ and
for $I, J\in \cal I$ with $I\neq J$ we have $I \cap J=\emptyset$.
For each interval $I\in{\cal I}$ there is a value $v_I$, and the value
of $C(x)=v_I$ if $x\in I$.

This is a very general definition, and to simplify reasoning about the
termination of algorithms a bit easier, we have as additional requirement that
each finite range $[b, e]$ is covered by finitely many intervals $I\in {\cal I}$

@ In practice there are 3 kind of calendars:
\orderedlist
\li Constant Calendars.  Any number can be a calendar with constant value.
\li Finite Calanders.  These calendars are specified by a finite number of values with a finite
    number of moments where the value changes.
\li Repetitive Calendars.  These have a regular pattern, like Monday-Friday every week forever.
    Or 09:00-17:00 every day.  Or Christmase etc.  Slightly more involved versions of these are
    09:00-17:00 eveyr day, taking daylight time savings into account, or easter.
\endorderedlist

@ The external API of a Calendar allows two things: Getting the value at
a certain moment, and iterating over the calendar to be able find the
break points.

@<API Definitions@>=
(defgeneric value-at             (calendar moment))
(defgeneric iterator-starting-at (calendar moment))

@ The iterator is the most fundamental way to work with calendars.
They are transient objects used for iterating through a Calander, but
they are also the only way to get duration information from the
calendar.  These iterators implement the following protocol

@<API Definitions@>=
(defgeneric iterator-move-next   (calendar-iterator))
(defgeneric iterator-value       (calendar-iterator))
(defgeneric iterator-time-span   (calendar-iterator))

@ The |iterator-move-next| will update the state so the iterator will
point to the next time span of the calendar.  The return value is
|nil| if the iterator is exhausted.

The |iterator-value| is the value of the Calendar for the time span |iterator-time-span|.

@ As mentioned before, numbers are calendars.  They implement the Calendar API by being the identity

@l
(defmethod value-at             ((value number) (moment t)) value)
(defmethod iterator-starting-at ((value number) (moment t)) value)

@ The iterator for a number calendar is the number itself.  This
requires that the implementation of the iterator interface is as follows

@l
(defmethod iterator-move-next  ((iterator number)) nil)
(defmethod iterator-value      ((iterator number)) iterator)
(defmethod iterator-time-span  ((iterator number)) +time-span-all+)

@ For more complicated Calendars there is this simple default
implementation of |value-at|, based on the iterators implementation.

@l
(defmethod value-at            ((calendar t) (moment t))
  (iterator-value (iterator-starting-at calendar moment)))

@ Of the more complicated calendars the following is the simplest.  It takes
a list of time spans with values.

In the implementation it is stored as an sorted array of |(cons
time-span value)| where the |time-spans| cover all of time.

@l
(defclass basic-calendar ()
  ((time-spans-with-value :type 'vector)))

@ Putting asside the construction for a moment, the implementation of
the iterator is quite straight forward.  The iterator keeps a
reference to the calendar and where it is in the list of time spans.

@l
(defclass basic-calendar-iterator ()
  ((calendar :type 'basic-calendar :initarg :calendar)
   (index    :type 'integer        :initarg :index)))

@ This will give the straightforward accessor implementations

@l
(defmethod iterator-value     ((iterator basic-calendar-iterator))
  (with-slots (index calendar) iterator
    (with-slots (time-spans-with-value) calendar
      (cdr (aref time-spans-with-value index)))))

(defmethod iterator-time-span ((iterator basic-calendar-iterator))
  (with-slots (index calendar) iterator
    (with-slots (time-spans-with-value) calendar
      (car (aref time-spans-with-value index)))))

@ The move next is just increasing the index, but we need to check if
we are at the end of the calendar

@l
(defmethod iterator-move-next ((iterator basic-calendar-iterator))
  (with-slots (index calendar) iterator
    (with-slots (time-spans-with-value) calendar
      (incf index)
      (< index (length time-spans-with-value)))))

@ The construction if the iterator requires finding the right index to
point to the timespan containing the |moment| given to the
|iterator-starting-at|.  We do this with a binary search.

@l
(defun binary-search (sequence key compare &optional (lb 0) (ub (length sequence)))
  (loop :while (> (- ub lb) 1)
     :for m = (floor (+ ub lb) 2)
     :do
       (if (funcall compare (aref sequence m) key)
	   (setf lb m)
	   (setf ub m)))
  (when (funcall compare (aref sequence lb) key)
    lb))

  
  

@*Identifiable Objects.  Identifiable objects are objects that can be
found by name or identifier.  They are typically created by the user
input as part of the plan specification.

These objects can be found by name or identifier.  There is no
requirement that the name is unique.  The identifier has to be unique
for the category of the object.  E.g. it is unique in the set of
events.  However the same identifier can be re-used for different
categories.

@<API Definitions@>=
(defgeneric name       (object))
(defgeneric identifier (object))

(defgeneric object-with-name  (name cateogry))
(defgeneric objects-with-name (name category))
(defgeneric object-with-identifier (identifier category))


@ A often used base class for these features is provided by

@<Base Classes@>=
(defclass identifiable ()
   ((name       :reader name       :initarg :name)
    (identifier :reader identifier :initarg :identifier)
    (category   :allocation :class :initarg :category)))
    
@*Index.

