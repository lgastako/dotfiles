# Copyright 2014 Google Inc. All Rights Reserved.
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

"""deployments describe command."""

from apitools.base.py import exceptions as apitools_exceptions

from googlecloudsdk.api_lib.deployment_manager import dm_v2_util
from googlecloudsdk.calliope import base
from googlecloudsdk.calliope import exceptions
from googlecloudsdk.command_lib.deployment_manager import dm_base


class _Results(object):
  """Encapsulate results into a single object to fit the Run() model."""

  def __init__(self, deployment, resources, outputs):
    self.deployment = deployment
    self.resources = resources
    self.outputs = outputs


class Describe(base.DescribeCommand, dm_base.DeploymentManagerCommand):
  """Provide information about a deployment.

  This command prints out all available details about a deployment.
  """

  detailed_help = {
      'DESCRIPTION': '{description}',
      'EXAMPLES': """\
          To display information about a deployment, run:

            $ {command} my-deployment
          """,
  }

  @staticmethod
  def Args(parser):
    """Args is called by calliope to gather arguments for this command.

    Args:
      parser: An argparse parser that you can use to add arguments that go
          on the command line after this command. Positional arguments are
          allowed.
    """
    parser.add_argument('deployment_name', help='Deployment name.')

  def Collection(self):
    return 'deploymentmanager.deployments_and_resources_and_outputs'

  def Format(self, args):
    """No need to list the id fields by default."""
    return self.ListFormat(args)

  def Run(self, args):
    """Run 'deployments describe'.

    Args:
      args: argparse.Namespace, The arguments that this command was invoked
          with.

    Returns:
      The requested Deployment.

    Raises:
      HttpException: An http error response was received while executing api
          request.
    """
    try:
      deployment = self.client.deployments.Get(
          self.messages.DeploymentmanagerDeploymentsGetRequest(
              project=self.project, deployment=args.deployment_name))
    except apitools_exceptions.HttpError as error:
      raise exceptions.HttpException(error, dm_v2_util.HTTP_ERROR_FORMAT)

    try:
      response = self.client.resources.List(
          self.messages.DeploymentmanagerResourcesListRequest(
              project=self.project, deployment=deployment.name))
      resources = response.resources
    except apitools_exceptions.HttpError:
      # Couldn't get resources, skip adding them to the table.
      # TODO(user): Why not raise HTTP exception here?
      resources = None

    outputs = []

    manifest = dm_v2_util.ExtractManifestName(deployment)

    if manifest:
      manifest_response = self.client.manifests.Get(
          self.messages.DeploymentmanagerManifestsGetRequest(
              project=self.project,
              deployment=args.deployment_name,
              manifest=manifest,
          )
      )
      # We might be lacking a layout if the manifest failed expansion.
      if manifest_response.layout:
        outputs = dm_v2_util.FlattenLayoutOutputs(manifest_response.layout)

    return _Results(deployment, resources, outputs)
