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
@<Time Spec API@>
@<Utility Functions@>
@<Base Classes@>

@*Moments, Events and Time Spans.  Project planning is based on time, and for planning there are two aspects of time important
\unorderedlist
\li A moment.  A specific moment in time.  This has no duration, but
    is used to identify the start, end or deadline of a task.
    Examples are {\it 2019-09-23 18:34 GMT}.  These are the focus of
    this section.
\li A time span.  A slice of time with a begin and end.  This has a
    duration. 
\endunorderedlist

These time specifications are the basis for more complex based time
objects such as calenders.  But these basic building blocks support
the following a basic API.  

@<Time Spec API@>=
(defgeneric before       (first second))
(defgeneric after        (first second))
(defgeneric time-between (first second))
(defgeneric time-span-between (first second))
(defgeneric duration     (object))
(defgeneric infinity-p   (object))
(defgeneric intersect    (first second))

@ The |before| and |after| methods are used to order time
specifications.  The general rule is that $a$ is before $b$ if the
complete time extent of $a$ happens before any time in $b$.

For moments this boils down to just comparing the time stamp.  For
intervals it means that one interval is completely before the other.
When comparing moments with intervals it does the similar thing, a
moment is before an interval if the moment is before the beginning of
the interval and an interval is before a moment when the moment is
after the end of the interval.  

The default implementation of the |after| method is in terms of the |before| method

@l
(defmethod after ((first t) (second t))
  (before second first))


@ The |time-between| calculates how much time is between two time specifications.
If there is overlap, the return value is 0.  If $a$ is before $b$ the call |(time-between a b)| is positive, 
if $b$ is before $a$ the return value is negative.

If the time between two time specifications would be infinite, the return value is |nil|.

@ The duration is only relevant for time spans, but works on moments as well.  The default implementation return $0$.

@l
(defmethod duration   ((object t)) 0)

@ The |infinity-p| call returns |t| if the time specification includes an infinity.  The default implementation is

@l
(defmethod infinity-p ((object t)) nil)

@ The |intersect| method returns a representation of a time-spec
that describes the intersection of the first and second argument.

@ Besides the methods above the following utility functions are useful, 
|first-of| and |last-of|.  They take a collection object and return 
the first respectively last element ordered by |before| from the collection.

@l
(defgeneric fist-of (collection))
(defgeneric last-of (collection))

@ The implementation is straight forward, but a little tedious.
@l
(defmethod first-of ((collection list))
  (loop
     :with result = nil
     :for item :in collection
     :when (or (not result) (before item result)) :do (setf result item)
     :finally (return result)))

(defmethod last-of ((collection list))
  (loop
     :with result = nil
     :for item :in collection
     :when (or (not result) (after item result)) :do (setf result item)
     :finally (return result)))


@2* Moments and Events. Moments are anonymous, they are purely
identified by the moment in time.  Events are special moments, they
have a name and can be identified by more than just the moment in
time.

@ For the presentation of moments we use universal time.  Which
imports the restrictions that we cannot represent time before the year
1900 and that the resolution is one second.  However, the choice of
time is limited to the |moment| class and hidden behind an interface.  So
we can later replace it, if needed, with another time representation.

@l 
(defclass moment ()
  ((timestamp :reader timestamp :initform nil :initarg :timestamp)))

@ In addition to normal timestamps, there are two special timestamps.
They are $-\infty$ an $+\infty$.

@l
(defvar +moment-before-time+ (make-instance 'moment :timestamp 'distant-past))
(defvar +moment-after-time+  (make-instance 'moment :timestamp 'distant-future))

@ These special moments can be identifed with a simple method

@l
(defmethod infinity-p ((object (eql +moment-before-time+))) t)
(defmethod infinity-p ((object (eql +moment-after-time+))) t)

@ The intersection of two moments is very easy.

@l
(defmethod intersect ((first moment) (second moment))
  (when (eql (timestamp first) (timestamp second))
    first))

@ Between two moments there passes a certain amount of time.  This is
determined by the |time-between| method.  If the first argument is
after the second argument the number will be negative.  If the
duration is infinite, the returned value is nil.

@l
(defmethod time-between ((first-moment moment) (second-moment moment))
    (if (or (infinity-p first-moment)
            (infinity-p second-moment))
	nil
	(- (slot-value second-moment 'timestamp)
	   (slot-value first-moment 'timestamp))))

@ Obviously timestamps can also be ordered.  The primary method is
|before| which indicates if the first argument happens before the
second.

@l

(defmethod before ((first-moment moment) (second-moment moment))
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
    (t nil)))

@ Before we delve into how to create moments and convert them to and from a string,
And an event is a moment, but has a name and an identitity.  

@l
(defclass event (moment identifiable)
  ()
  (:default-initargs
    :category 'event))

@2* Timestamp Specifications.  Timestamps are parsed by the |LOCAL-TIME| package.

The external representation of |moment| is handled by the package |LOCAL-TIME|

@<Required Packages@>=(require "LOCAL-TIME")

@ @<Package Imports@>=(:import-from "LOCAL-TIME" "PARSE-TIMESTRING")

@2* Time Spans. Time spans represent a half open range of time $[b,
e)$ with $b$ and $e$ moments.  They conceptually form the basis for
the planning buckets and calendars.

@l
(defclass time-span ()
  ((begin :type moment :reader begin :initform +moment-before-time+ :initarg :begin)
   (end   :type moment :reader end   :initform +moment-after-time+  :initarg :end)))

@ Also we will use identifiable time-span object in the construction of Calendars and Buckets.

@l
(defclass named-time-span (time-span identifiable)  ())

@ The time spans that are infinitely long are special

@l
(defmethod infinity-p ((time-span time-span))
  (or (infinity-p (begin time-span))
      (infinity-p (end time-span))))


@ In addition, the time span that covers all of time is used a bit and
therefore gets its own name.

@l
(defvar +time-span-all+ (make-instance 'time-span))

@ They time spans have have also a duration

@l
(defmethod duration ((time-span time-span))
  (time-between (begin time-span) (end time-span)))

@ And can be compared with the |before| and |after| with a |moment|.
We consider a time span before a moment if the whole of the time span
is before the moment, or in other words, if the end moment of the
time span lies before the checked moment.

TODO: Check again.  time-spans are half open intervals, so this is wrong.

@l
(defmethod before ((time-span time-span) (moment moment))
  (before (end time-span) moment))

@ Similarly a moment is before a time span if the moment is before the whole time span.

@l
(defmethod before ((moment moment) (time-span time-span))
  (before moment (begin time-span)))

@ Also a |time-span| is before another |time-span| if the whole of the first time span is before
the second time span.  That is, there is no intersection and the first time span is before the beginning of the second time-span

@l
(defmethod before ((first time-span) (second time-span))
  (before first (begin second)))

@ The after methods work out of the box with the default implementation.  The call |(after moment time-span)|
is translated to |(before time-span moment)|, which in turn is |(before (end time-span) moment)| and that is |(after moment (end time-span))|.

This is what we want, it returns true iff the moment is after the end of time-span.

Similarly the call |(after time-span moment)| is only true if the moment is before the beginning of of the time span.

@3 Intersections.
The intersection method of a |time-span| with a |moment| returns the
moment if the moment falls in the |time-span| and |nil| otherwise.

@l
(defmethod intersect ((time-span time-span) (moment moment))
  (when (and (before (begin time-span) moment)
	     (before moment (end time-span)))
    moment))

@ And for symmetry we need to implement the method the other way around as well.

@l
(defmethod intersect ((moment moment) (time-span time-span))
  (intersect time-span moment))

@ The intersection of two |time-span| objects is straight forward

@l
(defmethod intersect ((first time-span) (second time-span))
  (cond
    ((and (before (begin first) (begin second))
	  (before (end second) (end first)))
     second)
    ((and (before (begin second) (begin first))
	  (before (end first) (end second)))
     first)
    (t  (let ((begin (last-of `(,(begin first) ,(begin second))))
	      (end   (first-of `(,(end first) ,(end second)))))
	  (when (before begin end)
	    (make-instance 'time-span :begin begin :end end))))))

@ The timespan between two objects is a time span that starts at the end
of the first argument and ends at the begin of the second argument.

If there is no gap between the first and second argument, it will return |nil|.
It will also return |nil| if the second argument is before the first.

Basically it will return |nil| if |(between first second)| is not
greater than 0.   And it will return a time span of |(between first second)| if it is.

@l
(defmethod time-span-between ((first t) (second t))
  (when (before first second)
    (make-instance 'time-span :begin (end first) :end (begin second))))

@3* Checks on Timespans. 
Sometimes it is important to know if a vector of time-spans
are continuous covering an stretch of time.
This the following function |time-spans-have-no-gaps-p| will do, it will return
a generarlized true value if the range is continuous, it will return |nil| otherwise.

The argument is assumed to be sorted, so only consecutive values need to be checked.

Note that the test to check for the zero duration is |eql|.  This
becuase the |time-between| call can return |nil|

@l
(defun time-spans-have-no-gaps-p (sorted-list-of-time-spans)
  (loop
     :for (a b) :on sorted-list-of-time-spans
     :while b
     :unless (eql 0 (time-between (end a) (begin b)))
     :do (return-from time-spans-have-no-gaps-p nil))
  t)

@ Similarly, there is a check if the time spans are disjunct.

@l
(defun time-spans-have-no-overlap-p (sorted-list)
  (loop
     :for (a b) :on sorted-list
     :while b
     :when (intersect a b)
     :do (return-from time-spans-have-no-overlap-p nil))
  t)

@ And also to check if some time spans have zero width.

@l 
(defun time-spans-have-no-zero-width-p (list)
  (find 0 list :key #'duration))


@2* Time and Values.  Often we want to associate a single value with a
moment or time span.   There are two obvious strategies for implementing this:
\unorderedlist
\li Introducing a new object, which pairs a time-spec with a value.
\li Use a mixin-class for the value and define new sub-classes with the mixin.
\endunorderedlist

The the mixin-class approach has the advantage that it retains all the
original methods and functionality.  So we do not have to implement
each time based method for the new class.  The disadvantage of the
mixin-class approach is that each time we associate a value to a time
based object, the time based part of the new sub class need to be
recreated.

However, for each time based class we need to define the corresponding
mixin class.  this feels more of a burden than the new object
approach.

@l
(defclass time-spec-with-value ()
  ((time-spec  :reader time-spec :initarg :time-spec)
   (value      :reader value     :initarg :value)))

@ To construct we could use the |make-instance| call, but that becomes
quite verbose.  So we introduce the following shorthand

@<API Definitions@>=(defgeneric combine (first second))

@ With the following default implementations

@l
(defmethod combine ((time-spec moment) (value t))
  (make-instance 'time-spec-with-value :time-spec time-spec :value value))

(defmethod combine ((time-spec time-span) (value t))
  (make-instance 'time-spec-with-value :time-spec time-spec :value value))

@ Secondly, the time related methods defined in section |@<Time Spec API@>| are implemented.

@l
(defmacro create-wrapped-methods-1 (method-name type un-wrap)
  `(defmethod ,method-name ((first ,type)) (,method-name (,un-wrap first))))

(defmacro create-wrapped-methods-2 (method-name type un-wrap)
  `(progn
     (defmethod ,method-name ((first ,type) (second t)) (,method-name (,un-wrap first) second))
     (defmethod ,method-name ((first t) (second ,type)) (,method-name first (,un-wrap second)))))

(create-wrapped-methods-2 before time-spec-with-value time-spec)
(create-wrapped-methods-2 time-between time-spec-with-value time-spec)
(create-wrapped-methods-1 duration time-spec-with-value time-spec)
(create-wrapped-methods-1 infinity-p time-spec-with-value time-spec)

@3* Adding Misisng Data.  Typically data is only specified for
time-spans where the value is not zero or some other default value.
For the |basic-calendar| and some other constructs, we need to fill in
the missing sections.  The generic way of doing this is by using
calendars.  (See section on Calendars).

However this creates a bit of chicken and egg problem, because
calendars represent all of time, with no gap.

So we need a basic function to augment a list of
|time-spec-with-value| objects with the missing time intervals.

@l
(defun add-missing-time-specs-with-value (sorted-list-of-time-specs default-value)
  @<Handle empty list argument@>
  (let ((result (list)))
    @<Handle first time-spec@>
    (loop
       :for (a b) :on sorted-list-of-time-specs
       :while a
       :for time-span-between = (time-span-between a b)
       :do
	 (if b
	     (progn
	       (when time-span-between
		 (push (make-instance 'time-spec-with-value
				      :time-spec time-span-between :value default-value)
		       result))
	       (push b result))
	   @<Handle last time-spec@>))
    (nreverse result)))

@ Handling empty list argument is easy, just return a list containing one element covering all of time.

@<Handle empty list argument@>=
(unless sorted-list-of-time-specs
  (return-from add-missing-time-specs-with-value
    (list
     (make-instance 'time-spec-with-value
		    :time-spec +time-span-all+ :value default-value))))

@ Handling the first of the input list is slightly more code.  We need
to check that the first added time-span did not start at
|+moment-before-time+| and add a new |time-spec-with-value| covering the
initial stretch of time

@<Handle first time-spec@>=		;
(unless (eql +moment-before-time+ (begin (first sorted-list-of-time-specs)))
  (push (make-instance 'time-spec-with-value
		       :time-spec (make-instance 'time-span
						 :begin +moment-before-time+
						 :end (begin (first sorted-list-of-time-specs)))
		       :value default-value)
	result))


@ Handling the last of the input list is basically the reverse of handling the first |time-spec|.

@<Handle last time-spec@>=
(unless (eql +moment-after-time+ (end a))
  (push (make-instance 'time-spec-with-value
		       :time-spec (make-instance 'time-span
						 :begin (end a)
						 :end +moment-after-time+)
		       :value default-value)
	result))


@*Calendars.  Calendars are central to the simple-plan system.
Calendars control when a product can be made, when a ingredient is
available, control when people are available etc.  In addition they
determine the efficiency of production and can be used as an
indication of cost/benefit.

For this Calendars are implemented as a piecewise constant function
$C$ over time.  To be a bit more precise, the time range
$[-\infty,\infty)$ is divided up by a countable collection ${\cal I}$
of half open intervals.  That is, each interval $I\in {\cal I}$ is of
the form $I=[s_I, e_I)$. Also $(-\infty,\infty) \subset \cup_{I\in {\cal I}} I$ and
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
    09:00-17:00 every day, taking daylight time savings into account, or easter.
\endorderedlist

@ The external API of a Calendar allows two things: Getting the value at
a certain moment, and iterating over the calendar to be able find the
break points and values.

@<API Definitions@>=
(defgeneric value-at             (calendar moment))
(defgeneric iterator-starting-at (calendar moment))

@ The |iterator-starting-at| will take two elements, the calendar we
want to iterate over and the moment where the iteration starts.
Because calendars can have infinite number of change point, the moment
can in general not be $-\infty$.  Similarly, the value at $+\infty$ is
also not well defined.  

However for constant value calendars those values are defined and one
should {\bf not} count on the fact that |iterator-start-at| for
$+\infty$ returns |nil|.

@ The iterator is the most fundamental way to work with calendars.
They are transient objects used for iterating through a Calander, but
they are also the only way to get duration information from the
calendar.  These iterators implement the following protocol

@<API Definitions@>=
(defgeneric iterator-next        (calendar-iterator))
(defgeneric iterator-value       (calendar-iterator))
(defgeneric iterator-time-span   (calendar-iterator))


@ The |iterator-next| will return a new iterator that will
point to the next time span of the calendar.  The return value is
|nil| if the iterator is exhausted.

The |iterator-value| is the value of the Calendar for the time span
|iterator-time-span|.

The |iterator-value| and |value-at| of the calendar have the following relation

@l
(defmethod value-at            ((calendar t) (moment t))
  (iterator-value (iterator-starting-at calendar moment)))

@ This is also the default impementation of |value-at| for a calendar,
but more efficient version can be implemented as needed.

@ As mentioned before, numbers are calendars.  They implement the
Calendar API by being the identity

@l
(defmethod value-at             ((value number) (moment t)) value)
(defmethod iterator-starting-at ((value number) (moment t)) value)

@ The iterator for a number calendar is the number itself.  This
requires that the implementation of the iterator interface is as follows

@l
(defmethod iterator-next       ((iterator number)) nil)
(defmethod iterator-value      ((iterator number)) iterator)
(defmethod iterator-time-span  ((iterator number)) +time-span-all+)

@ The next simplest calendar takes a finite list of time spans with
values.

In the implementation it is stored as an sorted array of |time-spec-with-value|.  
The |time-spec| part of the content of the array need to cover all of time.

@l
(defclass basic-calendar ()
  ((time-spans-with-value :type 'vector :reader time-spans-with-value
			  :initarg :time-spans-with-value)))

@ Putting aside the construction for a moment, the implementation of
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
(defmethod iterator-next ((iterator basic-calendar-iterator))
  (with-slots (index calendar) iterator
    (with-slots (time-spans-with-value) calendar
      (when (< (+ index 1) (length time-spans-with-value))
	(make-instance 'basic-calendar-iterator :calendar calendar :index (+ index 1))))))

@ The construction if the iterator requires finding the right index to
point to the timespan containing the |moment| given to the
|iterator-starting-at|.  We do this with a binary search.

The constraints that the timespans are sorted an cover the whole
range, allows us to efficiently construct the iterator.

If the intervals are $[-\infty, e_0), [b_1, e_1), \dots, [b_n, +\infty)$
and the moment is $m$, the binary search will return the index of the interval
$[b_i, e_i)$ such that 

@l
(defmethod iterator-starting-at ((calendar basic-calendar) (moment moment))
  (make-instance 'basic-calendar-iterator
		 :calendar calendar
		 :index
		 (binary-search (time-spans-with-value calendar)
				moment
				#'before)))

@ The code above uses a binary search method that will search in an ordered array for a key.
To explain what this function does, let the array be written as $a_0, a_1, \dots, a_n$,
the key to look for as $k$ and the compare function as $<$.  

The return value is basically the last index $i$ for which $a_i < k$.

To be more precise:

\orderedlist
\li The array contains no elements, in  that case the return value is |nil|
\li It is not true that $a_0 < k$, in that case the return value is |nil|
\li It is true that $a_n < k$, in that case the return value is $n$.
\li The normal case, let $i$ such that $a_i < k$ and NOT $a_{i+1} < k$.  In that case the return value is $i$.
\endorderedlist

For this function to work, the assumption is that the array is ordered.  Ordered means 
that for each $k$ that you want to look up the following holds:  $a_i < k$ implies $a_{i-1} < k$.

@<Utility Functions@>=
(defun binary-search (sequence key compare &optional (lb 0) (ub (length sequence)))
  (loop :while (> (- ub lb) 1)
     :for m = (floor (+ ub lb) 2)
     :do
       (if (funcall compare (aref sequence m) key)
	   (setf lb m)
	   (setf ub m)))
  (when (and (< lb ub)
	     (funcall compare (aref sequence lb) key))
    lb))

@ The construction of a |basic-calendar| is based on a list of
|time-spec-with-value| objects and a default value.

The construction will check that the list of |time-spec-with-value|
objects are disjoint and fill in the missing time spans with the
default value.

An error is raised if the list time spans is not disjunct, or 
if the list contains time spans with zero duration.

The later is not needed, they can safely be dropped, but it could
point to an error in the calling code.  Until we discover a good use case for
zero duration intervals (including moments) we will raise an error.

@l
(defun basic-calendar (list-of-time-specs-with-value &optional (default-value 0))
  (let ((sorted-list (sort list-of-time-specs-with-value #'before)))
    (and
     (time-spans-have-no-zero-width-p sorted-list)
     (time-spans-have-no-overlap-p sorted-list)
     (make-instance 'basic-calendar
		    :time-spans-with-value
		    (coerce 
		     (add-missing-time-specs-with-value sorted-list default-value)
		     'vector)))))


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



