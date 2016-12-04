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

"""bigtable clusters delete command."""

from googlecloudsdk.api_lib.bigtable import util
from googlecloudsdk.calliope import base
from googlecloudsdk.core import log


@base.ReleaseTracks(base.ReleaseTrack.ALPHA)
class DeleteCluster(base.DeleteCommand):
  """Delete a Bigtable cluster (along with all its data)."""

  @staticmethod
  def Args(parser):
    """Register flags for this command."""
    util.AddClusterIdArgs(parser)

  def Run(self, args):
    """This is what gets called when the user runs this command.

    Args:
      args: an argparse namespace. All the arguments that were provided to this
        command invocation.

    Returns:
      Some value that we want to have printed later.
    """
    cli = self.context['clusteradmin']
    msg = (self.context['clusteradmin-msgs'].
           BigtableclusteradminProjectsZonesClustersDeleteRequest(
               name=util.ClusterUrl(args)))
    result = cli.projects_zones_clusters.Delete(msg)
    log.DeletedResource(args.cluster, kind='cluster',
                        details='in zone [{0}]'.format(args.zone))
    return result
