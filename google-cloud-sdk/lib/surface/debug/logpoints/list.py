# Copyright 2016 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""List command for gcloud debug logpoints command group."""

import argparse
import datetime

from googlecloudsdk.api_lib.debug import debug
from googlecloudsdk.calliope import arg_parsers
from googlecloudsdk.calliope import base
from googlecloudsdk.core import log
from googlecloudsdk.core import properties
from googlecloudsdk.core.util import times


class List(base.ListCommand):
  """List debug logpoints."""

  detailed_help = {
      'DESCRIPTION': """\
          This command displays a list of the active debug logpoints for a
          Cloud Debugger debug target.
      """
  }

  @staticmethod
  def Args(parser):
    base.URI_FLAG.RemoveFromParser(parser)
    parser.add_argument(
        'id_or_location_regexp', metavar='(ID|LOCATION-REGEXP)', nargs='*',
        help="""\
            Zero or more logpoint IDs, resource identifiers, or regular
            expressions to match against logpoint locations. If present, only
            logpoints matching one or more of these values will be displayed.
        """)
    parser.add_argument(
        '--all-users', action='store_true', default=True,
        help="""\
            If false, display only logpoints created by the current user.
        """)
    parser.add_argument(
        '--include-expired', action='store_true', default=False,
        help=argparse.SUPPRESS)
    parser.add_argument(
        '--include-inactive', default=300,
        type=arg_parsers.BoundedInt(lower_bound=0, unlimited=True),
        help="""\
            Include logpoints which failed or expired in the last
            INCLUDE_INACTIVE seconds. If the value is "unlimited", all failed
            or expired logpoints will be included.
        """)

  def Run(self, args):
    """Run the list command."""
    if args.include_expired:
      # The (deprecated, hidden) --include-expired argument is equivalent to
      # --include_inactive=unlimited
      log.warn('The --include-expired flag has been deprecated. Please use '
               '--include-inactive=unlimited instead.')
      args.include_inactive = None
    project_id = properties.VALUES.core.project.Get(required=True)
    debugger = debug.Debugger(project_id)
    debuggee = debugger.FindDebuggee(args.target)
    logpoints = debuggee.ListBreakpoints(
        args.id_or_location_regexp, include_all_users=args.all_users,
        include_inactive=(args.include_inactive != 0),
        restrict_to_type=debugger.LOGPOINT_TYPE)

    # Filter any results more than include_inactive seconds old.
    # include_inactive may be None, which means we do not want to filter the
    # results.
    if args.include_inactive > 0:
      cutoff_time = (times.Now(times.UTC) -
                     datetime.timedelta(seconds=args.include_inactive))
      logpoints = [lp for lp in logpoints if _ShouldInclude(lp, cutoff_time)]

    return logpoints

  def Collection(self):
    return 'debug.logpoints'


def _ShouldInclude(logpoint, cutoff_time):
  """Determines if a logpoint should be included in the output.

  Args:
    logpoint: a Breakpoint object describing a logpoint.
    cutoff_time: The oldest finalTime to include for completed logpoints.
  Returns:
    True if the logpoint should be included based on the criteria in args.
  """
  if not logpoint.isFinalState or not logpoint.finalTime:
    return True
  final_time = times.ParseDateTime(logpoint.finalTime, tzinfo=times.UTC)
  return final_time >= cutoff_time
