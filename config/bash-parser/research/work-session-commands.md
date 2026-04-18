---
  1. Verify AWS credentials

  aws-vault exec oncall-production -- aws sts get-caller-identity

  2. Deploy infrastructure (run a script)

  aws-vault exec oncall-production -- bash scripts/deploy-migration-dashboard.sh

  3. Discover log groups by name pattern

  aws-vault exec oncall-production -- aws logs describe-log-groups \
    --query 'logGroups[*].logGroupName' --output text \
    | tr '\t' '\n' | grep -iE 'assignment|notification|appsync' | sort

  4. Filter CloudWatch log events (simple)

  aws-vault exec oncall-production -- aws logs filter-log-events \
    --log-group-name "/aws/lambda/oncall-notify-admins-on-accept-prod" \
    --region us-east-2 \
    --start-time $(python3 -c "import time; print(int((time.time() - 3*86400) * 1000))") \
    --filter-pattern "ERROR" \
    --query 'events[].message' --output text 2>&1 | head -200

  5. CloudWatch Logs Insights query (start-query + get-results)

  aws-vault exec oncall-production -- aws logs start-query \
    --log-group-names "/aws/lambda/HandleAssignmentRequests-production" \
    --start-time $(date -j -u -f "%Y-%m-%dT%H:%M:%S" "2026-03-04T00:00:00" +%s) \
    --end-time $(date -j -u -f "%Y-%m-%dT%H:%M:%S" "2026-03-04T09:00:00" +%s) \
    --query-string 'fields @timestamp, @message
  | filter @message like "9269f27b" or @message like "createAssignmentDay"
  | sort @timestamp asc
  | limit 200'

  6. Retrieve Logs Insights results (piped into Python for formatting)

  aws-vault exec oncall-production -- aws logs get-query-results \
    --query-id "4d8e758f-089b-4a59-98fe-d485ac0ac383" \
    | python3 -c "
  import sys, json
  data = json.load(sys.stdin)
  print(f'Status: {data[\"status\"]}')
  print(f'Records matched: {data[\"statistics\"][\"recordsMatched\"]}')
  for r in data['results'][:20]:
      ts = next(f['value'] for f in r if f['field'] == '@timestamp')
      msg = next(f['value'] for f in r if f['field'] == '@message')
      print(f'{ts}: {msg[:300]}')
  "

  7. List Lambda functions matching a pattern

  aws-vault exec oncall-production -- aws lambda list-functions \
    --output json | python3 -c "
  import sys, json
  data = json.load(sys.stdin)
  for fn in data['Functions']:
      name = fn['FunctionName']
      if 'incoming' in name.lower() or 'sms' in name.lower():
          apollo = fn.get('Environment', {}).get('Variables', {}).get('APOLLO_OPERATIONS', 'NOT SET')
          print(f'{name}: APOLLO_OPERATIONS={apollo}')
  "

  8. Get Lambda function configuration/environment variables

  aws-vault exec oncall-production -- aws lambda get-function-configuration \
    --function-name "oncall-incoming-notifications-stack-incoming-sms-prod" \
    --region us-east-1 \
    --query 'Environment.Variables' --output json | python3 -c "
  import sys, json
  data = json.load(sys.stdin)
  for k in sorted(data.keys()):
      if 'APOLLO' in k or 'OPERATION' in k or 'ENV' in k:
          print(f'{k}: {data[k]}')
  "

  9. Error categorization pipeline (grep + sort + uniq)

  aws-vault exec oncall-production -- aws logs filter-log-events \
    --log-group-name "/aws/lambda/oncall-notify-admins-on-accept-prod" \
    --region us-east-2 \
    --start-time $(python3 -c "import time; print(int((time.time() - 3*86400) * 1000))") \
    --filter-pattern "ERROR" \
    --query 'events[].message' --output text 2>&1 \
    | grep -oE '"errorType":"[^"]*"' | sort | uniq -c | sort -rn

  10. Cross-region log group discovery

  aws-vault exec oncall-production -- aws logs describe-log-groups \
    --region us-east-1 \
    --log-group-name-prefix "/aws/lambda/oncall-incoming" \
    --query 'logGroups[*].logGroupName' --output text

  ---[10:02 AM]  ---
  1. Run tests with pattern matching (npm + Jest)

  cd /Users/jefffarr/src/oncall_amplify && npm test -- --testPathPattern=punch-time-handler.test.ts --no-coverage

  2. Run tests with Vitest

  cd /Users/jefffarr/src/oncall_amplify/stacks/oncall-graphql-stack && \
    npx vitest run src/services/assignmentDay/create-assignment-day/create-assignment-day.unit.test.ts

  3. TypeScript type-checking (no emit)

  cd /Users/jefffarr/src/oncall_amplify/stacks/oncall-notifications-stack && npx tsc --noEmit 2>&1 | head -50

  4. Linting with ESLint

  npx eslint stacks/oncall-graphql-stack/src/context.ts 2>&1

  5. Format with Prettier

  npx prettier --write stacks/oncall-graphql-stack/src/context.ts \
    stacks/oncall-graphql-stack/src/context.unit.test.ts \
    --config=.prettierrc

  6. Run a Node script with inline code for DB queries

  cd /Users/jefffarr/src/oncall_amplify/scripts/validation && node -e "
  require('dotenv').config();
  const { Client } = require('pg');
  const client = new Client({ connectionString: process.env.DATABASE_URL });
  (async () => {
    await client.connect();
    const r = await client.query(\`
      SELECT table_schema, column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'AssignmentDay'
    \`);
    console.table(r.rows);
    await client.end();
  })().catch(e => { console.error(e.message); process.exit(1); });
  "

  7. Run a validation/comparison script

  cd /Users/jefffarr/src/oncall_amplify/scripts/validation && \
    node compare-assignment-day.js --start-date 2026-03-03 --end-date 2026-03-06

  8. pnpm with Nx monorepo (test + codegen)

  pnpm nx test apploi-oncall --testFile=state/reduxLocalStore/assignmentDay/reduxLocalStoreAssignmentDayActions.test.ts
  pnpm run generate-graphql:oncall

  9. npx tsx for TypeScript evaluation

  npx tsx --eval "import { getAssignmentDayRange } from './apps/apploi-oncall/src/graphql/custom-queries.js'; \
    console.log('type:', typeof getAssignmentDayRange, 'length:', getAssignmentDayRange.length);"

  10. Validate YAML with npx

  npx js-yaml /Users/jefffarr/src/oncall_amplify/openspec/config.yaml > /dev/null 2>&1 \
    && echo "Valid YAML" || echo "Invalid YAML"

  ---[10:05 AM] ---
  1. jq transformation — bulk update JSON config across environments

  jq '
    .devnew.categories.function.ApplyAutoSchedule.apolloOperations = "NONE" |
    .master.categories.function.ApplyAutoSchedule.apolloUrl = "https://graphql-ecs.oncallplatform.com/graphql-iam" |
    .stagingnew.categories.function.ApplyAutoSchedule.apolloOperations = "NONE" |
    ...' amplify/team-provider-info.json > tmp && mv tmp amplify/team-provider-info.json

  2. Codebase audit with shell loop + grep counting

  for f in stacks/oncall-graphql-stack/src/schema/models/*.ts; do
    name=$(basename "$f" .ts)
    [[ "$name" == *test* || "$name" == "index" ]] && continue
    has_dl=$(grep -c "dataloaders\|dataloader" "$f" 2>/dev/null || echo 0)
    has_prisma=$(grep -c "ctx.prisma\." "$f" 2>/dev/null || echo 0)
    echo "$name: dataloaders=$has_dl inline_prisma=$has_prisma"
  done

  3. GitHub Actions — fetch failed jobs with jq filtering

  gh run view 22147124603 --json jobs \
    --jq '.jobs[] | select(.conclusion == "failure") | {
      name: .name,
      steps: [.steps[] | select(.conclusion == "failure") | {name: .name}]
    }'

  4. GitHub PR — fetch Copilot review comments

  gh api repos/apploitech/oncall_amplify/pulls/2292/reviews \
    | jq '.[] | select(.user.login == "copilot-pull-request-reviewer") | .id' \
    | head -1 \
    | xargs -I {} gh api repos/apploitech/oncall_amplify/pulls/reviews/{}/comments

  5. find + sed pipeline — count TypeScript files by directory

  find stacks/oncall-graphql-stack/src -name "*.ts" \
    -not -path "*/node_modules/*" -not -name "*.test.*" \
    | sed 's|.*/src/||' | cut -d/ -f1 | sort | uniq -c | sort -rn

  6. GitHub Actions — search test failure logs

  gh run view 22315009136 --log --job 64557563273 2>&1 \
    | grep -A 5 -E "(FAIL|Error|✕|✗|×)" | head -150

  7. Python one-liner — summarize mismatch columns from JSONL report

  grep '"mismatch"' reports/assignment-day-*.jsonl | python3 -c "
  import sys, json, collections
  cols = collections.Counter()
  for line in sys.stdin:
      row = json.loads(line)
      for c in row.get('columns', {}):
          cols[c] += 1
  for col, count in cols.most_common():
      print(f'{col:40s} {count:>8,}')
  "

  8. Python one-liner — extract deploy config

  cat .github/workflows/deploy-stacks/deploy-stacks-config.json | python3 -c "
  import sys, json
  data = json.load(sys.stdin)
  for stack in data:
      if 'notifications' in json.dumps(stack).lower():
          print(json.dumps(stack, indent=2))
  "

  9. Shell loop — scan preset files for scope sections

  for f in config/gptel/presets/*.md; do
    echo "=== $(basename $f) ==="
    grep -n "^paths:\|^org_roam_patterns:\|^shell_commands:\|^scope_profile:" "$f" \
      || echo "  (no scope sections)"
  done

  10. GitHub PR patch extraction

  gh pr view 2029 --json files \
    --jq '.files[] | select(.path == "amplify/team-provider-info.json") | .patch' \
    | head -200

  ---[10:07 AM]  ---
  psql — Direct database queries

  source .env && psql "$DATABASE_URL" -f /tmp/check-meta-op.sql
  PGPASSWORD='...' psql -h oncall-psql-devnew-....rds.amazonaws.com \
    -U oc_admin_user -d oncall -c "
    SELECT id, name FROM core.\"Project\" WHERE \"companyId\" = '8' ORDER BY id;
  "

  find — Deep file discovery with filters

  find /Users/jefffarr/src/oncall_amplify -maxdepth 6 \
    -path "*/node_modules/.bin/prettier" 2>/dev/null | head -10
  find /Users/jefffarr/src/oncall_amplify/openspec/specs -type f -o -type d | sort \
    | while read f; do
      depth=$(echo "$f" | sed "s|.*/openspec/specs||" | tr -cd '/' | wc -c)
      indent=$(printf '%*s' $((depth * 2)) '')
      echo "${indent}$(basename "$f")"
    done

  Shell loop — survey stack structures

  for stack in oncall-notifications-stack oncall-integration-service-stack \
    oncall-api-stack oncall-data-sync-stack; do
    echo "=== $stack ==="
    ls stacks/$stack/src/ 2>/dev/null || ls stacks/$stack/ 2>/dev/null | head -10
    echo ""
  done

  Shell loop — iterate beads for bulk review

  for id in emacs-2s2 emacs-i0p emacs-12s emacs-iql emacs-mdp emacs-obt \
    emacs-le4 emacs-0xb emacs-yca emacs-and emacs-4y9; do
    echo "=== $id ===" && bd show $id 2>&1 && echo
  done

  Shell loop — batch comparison script across date ranges

  for entry in "2024-02-01 2024-03-01" "2024-03-01 2024-04-01" ...; do
    START=$(echo "$entry" | cut -d' ' -f1)
    END=$(echo "$entry" | cut -d' ' -f2)
    REPORT="reports/shift-${START}_${END}.jsonl"
    [ -f "$REPORT" ] && echo "SKIP: $REPORT" && continue
    echo "=== $START to $END ==="
    node compare-shift.js --start-date "$START" --end-date "$END"
  done

  grep — multi-file conflict marker detection

  grep -rn '<<<<<<<\|=======\|>>>>>>>' \
    stacks/oncall-graphql-stack/src/schema/models/AssignmentDay.ts \
    stacks/oncall-graphql-stack/src/services/assignmentDay/create-assignment-day/*.ts \
    2>/dev/null || echo "No conflict markers found"

  grep — mining zsh_history for past terraform commands

  grep -i 'terraform' ~/.zsh_history \
    | grep -iE 'devnew|prod|oncall_prod|oncall_staging' | tail -50

  git history archaeology

  for commit in e529461 e3d1638 a7dcab9 9849bb9 9f5616f 9a13663; do
    echo "=== $commit ==="
    git show "$commit:state/activities/apploi-mac/activities-activities" 2>/dev/null \
      | grep -oP '(?<=#\d+=")[^"]+' | sort -u
  done

  bash function + JSON generation (CloudWatch dashboard widget builder)

  bash -c '
  REGION="us-east-2"
  LOG_GROUPS_JSON='"'"'["/aws/lambda/CreateSplitAssignment-master"]'"'"'
  widget() {
    local title=$1 query=$2 x=$3 y=$4 w=$5 h=$6
    cat <<WIDGET
  { "type":"log", "x":${x}, "y":${y}, "width":${w}, "height":${h},
    "properties":{"title":"${title}","query":"${query}","region":"${REGION}",
    "sources":${LOG_GROUPS_JSON}} }
  WIDGET
  }
  echo "{\"widgets\":[$(widget "Test" "$Q1" 0 0 12 6)]}" | python3 -m json.tool
  '

  bd (beads) — bulk status/branch checking

  bd show oncall_amplify-cy8 2>/dev/null | grep -i "label\|branch" || true
  bd show oncall_amplify-nkz 2>/dev/null | grep -i "label\|branch" || true
  bd show oncall_amplify-d3a 2>/dev/null | grep -i "label\|branch" || true

  ---[10:09 AM]  ---
  Emacs batch + process substitution — verify tangle sync

  diff <(emacs --batch --no-init-file -l org --eval \
    '(progn (find-file "config/gptel/gptel.org") (org-babel-tangle))' \
    2>/dev/null && cat config/gptel/gptel.el) config/gptel/gptel.el

  Chained tangle validation — validate all modified .org files

  ./bin/tangle-org.sh config/gptel/preset-registration.org && \
  ./bin/tangle-org.sh config/gptel/scope-profiles.org && \
  ./bin/tangle-org.sh config/gptel/gptel.org && \
  ./bin/tangle-org.sh config/gptel/sessions/constants.org && \
  ./bin/tangle-org.sh config/gptel/sessions/filesystem.org && \
  ./bin/tangle-org.sh config/gptel/sessions/commands.org && \
  ./bin/tangle-org.sh config/gptel/sessions/branching.org && \
  ./bin/tangle-org.sh config/gptel/scope/scope-core.org && \
  ./bin/tangle-org.sh config/gptel/tools/persistent-agent.org

  Snippet file counting with xargs

  for dir in /Users/jefffarr/.emacs.d.backup-2026-01-28/snippets/*/; do
    count=$(find "$dir" -type f -not -path '*/.git/*' | wc -l)
    basename "$dir" | xargs -I {} echo "{}: $count files"
  done | sort

  git branch archaeology — show commits per worktree branch

  echo "=== agent-a5c43c7c (emacs-hed: gptel loader) ===" && \
    git log --oneline worktree-agent-a5c43c7c ^gptel-preset-alignment && \
  echo "=== agent-afc354a7 (emacs-t9t: scope profiles) ===" && \
    git log --oneline worktree-agent-afc354a7 ^gptel-preset-alignment && \
  echo "=== agent-af123356 (emacs-nli: scope core) ===" && \
    git log --oneline worktree-agent-af123356 ^gptel-preset-alignment

  Bulk worktree cleanup

  for wt in .claude/worktrees/agent-*; do
    git worktree remove --force "$wt" 2>/dev/null
  done && git worktree list

  Cherry-pick conflict resolution pipeline

  git checkout --ours config/gptel/sessions/constants.el && \
  git add config/gptel/sessions/constants.el && \
  ./bin/tangle-org.sh config/gptel/sessions/constants.org && \
  git add config/gptel/sessions/constants.org config/gptel/sessions/constants.el && \
  git cherry-pick --continue --no-edit

  git for-each-ref — check which branches track a file

  git for-each-ref --format='%(refname:short)' refs/heads | while read branch; do
    if git ls-tree "$branch" -- .beads/issues.jsonl &>/dev/null && \
       git ls-tree "$branch" -- .beads/issues.jsonl | grep -q issues.jsonl; then
      echo "$branch: tracked"
    fi
  done

  find + xargs + grep — cross-repo codebase search

  find /Users/jefffarr/src/oncall_amplify -path "*/node_modules" -prune \
    -o -type f \( -name "*.ts" -o -name "*.js" \) -print \
    | xargs grep -l "CreateAssignmentDay\|createAssignmentDay" 2>/dev/null

  find + xargs — search terraform files across repos

  find /Users/jefffarr/src/stadium/terraform -type f -name "*.tf" \
    | xargs grep -l "idle_timeout\|deregistration_delay\|connection_draining"

  git show + grep — inspect functions at a specific commit

  cd runtime/straight/repos/gptel && \
    git show 11ddace:gptel.el | grep -n \
    "gptel--restore-state\|gptel--save-state\|gptel--apply-preset\|gptel-get-preset\|gptel--known-presets"

  Jira CLI availability check

  which jira 2>/dev/null || brew list --formula | grep jira 2>/dev/null
  jira --version 2>/dev/null
  echo "---"
  jira me 2>/dev/null || echo "jira cli not configured or not found"

  git log search — find commits by keyword

  git log --oneline --all \
    --grep="notifications\|CreateAssignmentDay\|ConfirmAssignmentDay" | head -20

  ---[10:10 AM]  ---
  AWS Services Used (8 distinct services, ~180 total calls)

  aws sts — Identity verification (4 calls)

  aws-vault exec oncall-production -- aws sts get-caller-identity

  aws appsync — API configuration inspection (1 call)

  aws-vault exec oncall-production -- aws appsync get-graphql-api \
    --region us-east-2 \
    --api-id paidbaab7vg6pkly5x3ljw3zdi \
    --query 'graphqlApi.logConfig' --output json

  aws ecs — Service and task introspection (4 calls)

  # Describe service status
  aws-vault exec oncall-production -- aws ecs describe-services \
    --cluster schedule-ecs-prod --services schedule-apollo-graphql-prod \
    --region us-east-2 \
    --query 'services[0].{status:status,taskDefinition:taskDefinition,desiredCount:desiredCount,runningCount:runningCount}'

  # Get container log config from task definition
  aws-vault exec oncall-production -- aws ecs describe-task-definition \
    --task-definition schedule-apollo-graphql-prod:358 --region us-east-2 \
    --query 'taskDefinition.containerDefinitions[*].{name:name,logDriver:logConfiguration.logDriver,logOptions:logConfiguration.options}'

  # List running tasks
  aws ecs list-tasks --cluster schedule-ecs-prod \
    --service-name schedule-apollo-graphql-prod --region us-east-2

  aws events — EventBridge inspection (4 calls)

  # Nested subshell: describe rule using dynamic bus name lookup
  aws-vault exec oncall-production -- aws events describe-rule \
    --name "oncall-notify-admins-on-accept-prod-rule" \
    --event-bus-name $(aws-vault exec oncall-production -- aws events list-event-buses \
      --region us-east-2 --query 'EventBuses[?starts_with(Name, `oncall`)].Name' --output text) \
    --region us-east-2

  # Get target retry config
  aws-vault exec oncall-production -- aws events list-targets-by-rule \
    --rule "oncall-notify-admins-on-accept-prod-rule" \
    --event-bus-name "oncall-dynamo-stream-event-bus-prod" --region us-east-2

  aws lambda — Function discovery and configuration (20 calls)

  # List + filter with JMESPath
  aws-vault exec oncall-production -- aws lambda list-functions \
    --query 'Functions[?contains(FunctionName, `notifications-stack`) || contains(FunctionName, `notification-stack`)].{Name: FunctionName}' \
    --output json | python3 -m json.tool

  # List + filter with Python post-processing
  aws-vault exec oncall-production -- aws lambda list-functions --output json | python3 -c "
  import sys, json
  data = json.load(sys.stdin)
  for fn in data['Functions']:
      name = fn['FunctionName']
      if 'incoming' in name.lower() or 'sms' in name.lower():
          apollo = fn.get('Environment',{}).get('Variables',{}).get('APOLLO_OPERATIONS','NOT SET')
          print(f'{name}: APOLLO_OPERATIONS={apollo}')
  "

  # Get env vars from specific function (cross-region)
  aws-vault exec oncall-production -- aws lambda get-function-configuration \
    --function-name "oncall-incoming-notifications-stack-incoming-sms-prod" \
    --region us-east-1 --query 'Environment.Variables' --output json | python3 -c "..."

  # Check retry configuration
  aws-vault exec oncall-production -- aws lambda get-function-event-invoke-config \
    --function-name oncall-notify-admins-on-accept-prod --region us-east-2

  aws logs — CloudWatch (146 calls, dominant service)

  describe-log-groups — Discovery (34 calls)
  # Simple grep filter
  aws-vault exec oncall-production -- aws logs describe-log-groups \
    --query 'logGroups[*].logGroupName' --output text \
    | tr '\t' '\n' | grep -iE 'appsync' | sort

  # Loop to check existence of multiple log groups
  for fn in CreateSplitAssignment CloseProject ApplyAutoSchedule ...; do
    aws-vault exec oncall-production -- aws logs describe-log-groups \
      --log-group-name-prefix "/aws/lambda/${fn}" \
      --query 'logGroups[].logGroupName' --output text --region us-east-2
  done | tr '\t' '\n' | sort

  filter-log-events — Direct log search (51 calls)
  # Loop over IDs to search for each one
  for id in 1aedb946-... 41237ccb-...; do
    echo "=== $id ==="
    aws-vault exec oncall-production -- aws logs filter-log-events \
      --log-group-name "/aws/lambda/oncall-notify-admins-on-accept-prod" \
      --region us-east-2 \
      --start-time $(python3 -c "import time; print(int((time.time() - 3*86400) * 1000))") \
      --filter-pattern "\"$id\"" \
      --query 'events[].[timestamp, message]' --output text | grep -c ""
  done

  # Extract structured data from CURL logs
  aws-vault exec oncall-production -- aws logs filter-log-events \
    --region us-east-1 \
    --log-group-name "/aws/lambda/oncall-incoming-notifications-stack-incoming-sms-prod" \
    --start-time $(date -v-1H +%s000) \
    --filter-pattern "CURL COMMAND" --limit 10 --output json \
    | python3 -c "
  import json, sys, re, datetime
  data = json.load(sys.stdin)
  for e in data.get('events', []):
      ts = datetime.datetime.fromtimestamp(e['timestamp']/1000).isoformat()
      match = re.search(r\"-d '(.+)'\", e['message'], re.DOTALL)
      if match:
          payload = json.loads(match.group(1))
          print(f'{ts}: variables={json.dumps(payload.get(\"variables\", {}))}')
  "

  start-query + get-query-results — CloudWatch Insights (34 + 35 calls)
  # Multi-query batch: save all results then process
  aws-vault exec oncall-production -- bash -c '
  aws logs get-query-results --query-id "2a395fde-..." --output json > /tmp/q1.json
  aws logs get-query-results --query-id "45c969fb-..." --output json > /tmp/q2.json
  aws logs get-query-results --query-id "7757f642-..." --output json > /tmp/q3.json
  '

  # Formatted table output via Python
  aws-vault exec oncall-production -- aws logs get-query-results \
    --query-id "5e1f1d14-..." --output json | python3 -c "
  import json, sys
  data = json.load(sys.stdin)
  fmt = '{:<45s} {:<15s} {:<15s} {}'
  print(fmt.format('functionName','mode','stage','operations'))
  for row in data['results']:
      d = {f['field']: f['value'] for f in row if f['field'] != '@ptr'}
      print(fmt.format(d.get('functionName',''), d.get('apolloOperationsMode',''), ...))
  "

  get-log-events — Raw log stream access (6 calls)
  # Nested subshell: find log stream name then fetch events from it
  aws-vault exec oncall-production -- aws logs get-log-events \
    --region us-east-1 \
    --log-group-name "/aws/lambda/oncall-incoming-notifications-stack-incoming-sms-prod" \
    --log-stream-name "$(aws-vault exec oncall-production -- aws logs filter-log-events \
      --region us-east-1 \
      --log-group-name '...' \
      --start-time $(date -v-1H +%s000) \
      --filter-pattern 'a79f427d' --limit 1 \
      --query 'events[0].logStreamName' --output text)" \
    --start-time $(( $(date -v-1H +%s) * 1000 )) --output json \
    | python3 -c "..."

  tail — Live/recent log streaming (16 calls)
  aws-vault exec oncall-production -- aws logs tail \
    "/aws/ecs/schedule-apollo-graphql-prod" \
    --region us-east-2 --since 6h --format short \
    --filter-pattern '"prisma:error"'

  ---[10:12 AM]---
  10 Distinct Patterns

  1. Epoch timestamp calculation (inline, most common)

  python3 -c "import time; print(int((time.time() - 3*86400) * 1000))"
  Used inside $() for --start-time in 23+ aws logs commands.

  2. CloudWatch Insights result formatter (tabular)

  aws ... get-query-results ... | python3 -c "
  import json, sys
  data = json.load(sys.stdin)
  print('Status:', data['status'])
  fmt = '{:<45s} {:<15s} {:<15s} {}'
  print(fmt.format('functionName', 'mode', 'stage', 'operations'))
  print('-' * 90)
  for row in data['results']:
      d = {f['field']: f['value'] for f in row if f['field'] != '@ptr'}
      print(fmt.format(d.get('functionName',''), d.get('apolloOperationsMode',''), ...))
  "

  3. CloudWatch Insights result formatter (time-series aggregation)

  python3 << 'PYEOF'
  import json
  for i, label in enumerate(["Q1: Activity by Function", "Q2: Migration Phase", ...], 1):
      with open(f"/tmp/q{i}.json") as f:
          d = json.load(f)
      dims = set()
      for r in d['results']:
          fields = {x['field']: x['value'] for x in r if x['field'] != '@ptr' and 'bin(' not in x['field']}
          dims.add(tuple(sorted(fields.items())))
      for dim in sorted(dims):
          total = sum(int(x['value']) for r in d['results'] for x in r
                       if x['field'] in ('operations','count()') and
                       tuple(sorted({...})) == dim)
          print(f"    {dim}: {total:,}")
  PYEOF

  4. Log event timestamp humanizer

  aws ... filter-log-events ... --output json | python3 -c "
  import json, sys, datetime
  data = json.load(sys.stdin)
  for e in data.get('events', []):
      ts = datetime.datetime.fromtimestamp(e['timestamp']/1000).isoformat()
      msg = e['message'].strip()
      if msg.startswith('REPORT') or msg.startswith('END'):
          continue
      print(f'{ts}: {msg[:1000]}')
  print(f'Total events: {len(data.get(\"events\", []))}')
  "

  5. Error deduplication with UUID normalization

  cat tool-results/logs.txt | python3 -c "
  import sys, json, re
  from collections import Counter
  error_types = Counter()
  for line in sys.stdin:
      m = re.match(r'^\d{4}-\d{2}-\d{2}T[\d:]+ (.+)', line.strip())
      if not m: continue
      try:
          j = json.loads(m.group(1))
          msg = j.get('msg', j.get('message', 'unknown'))
          msg = re.sub(r'[0-9a-f]{8}-[0-9a-f]{4}-...-[0-9a-f]{12}', '<UUID>', msg)
          msg = re.sub(r'company \d+', 'company <N>', msg)
          error_types[msg] += 1
      except:
          error_types[m.group(1)[:100]] += 1
  for msg, count in error_types.most_common(30):
      print(f'  [{count:>4}x] {msg}')
  "

  6. CURL log payload extractor (regex + JSON parse)

  aws ... filter-log-events ... --output json | python3 -c "
  import json, sys, re, datetime
  data = json.load(sys.stdin)
  for e in data.get('events', []):
      ts = datetime.datetime.fromtimestamp(e['timestamp']/1000).isoformat()
      match = re.search(r\"-d '(.+)'\", e['message'], re.DOTALL)
      if match:
          payload = json.loads(match.group(1))
          variables = payload.get('variables', {})
          print(f'{ts}: variables={json.dumps(variables)}')
  "

  7. JSON config file transformer (read/modify/write)

  python3 << 'EOF'
  import json
  with open('amplify/team-provider-info.json', 'r') as f:
      data = json.load(f)
  environments = ['devnew','master','stagingnew','bdrkdev','bdrkqa',...]
  for env in environments:
      if env in data:
          data[env]['categories']['function']['ApplyDepartmentMasterRotation']['apolloOperations'] = 'NONE'
          data[env]['categories']['function']['ApplyDepartmentMasterRotation']['apolloUrl'] = ''
  with open('amplify/team-provider-info.json', 'w') as f:
      json.dump(data, f, indent=2)
  print(f"Updated {len(updated)} environments")
  EOF

  8. JSONL report aggregator (column mismatch analysis)

  python3 -c "
  import json, collections
  cols = collections.Counter()
  combos = collections.Counter()
  with open('assignment-day-2026-03-04_2026-03-05.jsonl') as f:
      for line in f:
          row = json.loads(line)
          if row['type'] == 'mismatch':
              col_set = frozenset(row.get('columns', {}).keys())
              combos[tuple(sorted(col_set))] += 1
              for c in row.get('columns', {}):
                  cols[c] += 1
  for col, count in cols.most_common():
      print(f'  {col:40s} {count:>8,}')
  print()
  for combo, count in combos.most_common(15):
      print(f'  {count:>8,}  {list(combo)}')
  "

  9. CloudWatch dashboard JSON generator

  python3 << 'PYEOF' | python3 -m json.tool > /dev/null && echo "Valid JSON"
  import json
  REGION = "us-east-2"
  LOG_GROUPS = ["/aws/lambda/CreateSplitAssignment-master", ...]
  QUERIES = [
      {"title": "Activity by Lambda function",
       "query": 'fields @timestamp, functionName\n| stats count() as operations by functionName, bin(@timestamp, 5m)'},
      ...
  ]
  widgets = []
  for i, q in enumerate(QUERIES):
      source_prefix = " | ".join(f"SOURCE '{lg}'" for lg in LOG_GROUPS)
      widgets.append({"type":"log","x":0,"y":i*6,"width":24,"height":6,
          "properties":{"title":q["title"],"query":f"{source_prefix} | {q['query']}","region":REGION}})
  print(json.dumps({"widgets": widgets}))
  PYEOF

  10. Unique error instance extractor (dedup by normalized message)

  cat logs.txt | python3 -c "
  import sys, json, re
  seen = set()
  for line in sys.stdin:
      m = re.match(r'^(\d{4}-\d{2}-\d{2}T[\d:]+) (.+)', line.strip())
      if not m: continue
      ts, payload = m.group(1), m.group(2)
      try:
          j = json.loads(payload)
          msg = j.get('msg', j.get('message', ''))
          key = re.sub(r'[0-9a-f]{8}-...-[0-9a-f]{12}', '<UUID>', msg)
          if key not in seen:
              seen.add(key)
              print(f'=== {ts} ===')
              print(json.dumps(j, indent=2)[:800])
      except:
          pass
  "

  ---[10:14 AM] ---
  10 Distinct Patterns

  1. Inline DB query with dotenv + pg (most complex inline pattern)

  node -e "
  require('dotenv').config();
  const { Pool } = require('pg');
  const url = new URL(process.env.DATABASE_URL);
  url.searchParams.delete('schema');
  const pool = new Pool({ connectionString: url.toString(), max: 1 });

  pool.query(
    'SELECT id, \"companyId\", \"createdAt\", status FROM public.\"AssignmentDay\" WHERE id = ANY(\$1) ORDER BY \"createdAt\" ASC',
    [ids]
  ).then(r => {
    for (const row of r.rows) console.log(JSON.stringify(row, null, 2));
    pool.end();
  });
  "

  2. Inline DB introspection with console.table

  node -e "
  require('dotenv').config();
  const { Client } = require('pg');
  const client = new Client({ connectionString: process.env.DATABASE_URL });
  (async () => {
    await client.connect();
    const r = await client.query(\`
      SELECT table_schema, column_name, data_type, udt_name
      FROM information_schema.columns
      WHERE table_name = 'AssignmentDay'
        AND column_name IN ('id', '_originalRoleId')
      ORDER BY table_schema, column_name
    \`);
    console.table(r.rows);
    await client.end();
  })().catch(e => { console.error(e.message); process.exit(1); });
  "

  3. DB connectivity test (BEGIN/ROLLBACK probe)

  node -e "
  require('dotenv').config();
  const { Client } = require('pg');
  const client = new Client({ connectionString: process.env.DATABASE_URL });
  (async () => {
    await client.connect();
    console.log('Connected');
    await client.query('BEGIN');
    console.log('BEGIN ok');
    const r = await client.query('SELECT count(*) FROM core.\"AssignmentDay\" LIMIT 1');
    console.log('core.AssignmentDay count:', r.rows[0]);
    await client.query('ROLLBACK');
    console.log('ROLLBACK ok');
    await client.end();
  })().catch(e => { console.error('Error:', e.message); process.exit(1); });
  "

  4. Extract HTML templates from JS source (file parsing + code extraction)

  node -e "
  const fs = require('fs');
  const src = fs.readFileSync('EmailTemplateHandlerService.js', 'utf8');
  const templates = {};
  const templateFunctions = [
    '_getAbsenceRequestedNotificationEmailTemplate',
    '_getOverstaffNotificationEmailTemplate', ...
  ];
  for (const fn of templateFunctions) {
    const funcStart = src.indexOf('function ' + fn + '()');
    const returnStart = src.indexOf('return \`', funcStart);
    const closingBacktick = src.indexOf('\`;\n}', returnStart + 8);
    templates[fn] = src.substring(returnStart + 8, closingBacktick);
    console.log(fn + ': ' + templates[fn].length + ' chars');
  }
  fs.writeFileSync('/tmp/email-templates-raw.json', JSON.stringify(templates, null, 2));
  "

  5. JSONL report analyzer (read file, count types, group mismatch columns)

  node -e "
  const fs = require('fs');
  const lines = fs.readFileSync('reports/assignment-day-20260226-120059.jsonl', 'utf8').trim().split('\n');
  const rows = lines.map(l => JSON.parse(l));
  const types = {}, colSets = {};
  for (const r of rows) {
    types[r.type] = (types[r.type] || 0) + 1;
    if (r.type === 'mismatch') {
      const key = Object.keys(r.columns).sort().join(',');
      colSets[key] = (colSets[key] || 0) + 1;
    }
  }
  console.log('Types:', types);
  console.log('Column sets:', colSets);
  "

  6. Piped stdin reader (cat | node)

  cat reports/assignment-day-*.jsonl | grep '"missing_from_core"' | head -20 | node -e "
  const lines = require('fs').readFileSync('/dev/stdin','utf8').trim().split('\n');
  const shiftIds = new Set();
  for (const line of lines) {
    const row = JSON.parse(line);
    if (row.type === 'missing_from_core') shiftIds.add(row.id);
  }
  console.log('Sample missing AssignmentDay IDs:', [...shiftIds].slice(0,5));
  "

  7. Schema comparison script in batch loop (with skip logic)

  for entry in "2024-02-01 2024-03-01" "2024-03-01 2024-04-01" ...; do
    START=$(echo "$entry" | cut -d' ' -f1)
    END=$(echo "$entry" | cut -d' ' -f2)
    REPORT="reports/shift-${START}_${END}.jsonl"
    [ -f "$REPORT" ] && echo "SKIP: $REPORT" && continue
    node compare-shift.js --start-date "$START" --end-date "$END"
    sleep 5
  done

  8. Fix/migration script (dry-run and live)

  # Dry run - ROLLBACK all changes
  node fix-assignment-day.js reports/assignment-day-2024-01-01_2024-02-01.jsonl --dry-run

  # Live run - COMMIT changes
  node fix-assignment-day.js reports/assignment-day-2024-01-01_2024-02-01.jsonl

  9. Sandboxed function extraction from script (dynamic new Function)

  node -e "
  const script = require('fs').readFileSync('./fix-assignment-day.js', 'utf8');
  const m = {};
  const fn = new Function('module','exports','require','process','__dirname','__filename',
    script.replace('require(\"dotenv\").config();', '') +
    '; module.exports = { buildInsertSQL, updatePublicExpr, q };'
  );
  fn(m, m.exports = {}, require, {...process, exit: () => {}}, __dirname, __filename);
  console.log('=== INSERT SQL ===');
  console.log(m.exports.buildInsertSQL());
  "

  10. Column count cross-check (inline schema validation)

  node -e "
  const publicCols = ['id','__edb_e__','__edb_v__','__typename','_meta/op',
    'absenceNotes','absenceSubType', ... /* 64 columns */];
  const coreCols = ['id','__typename','_meta/op','absenceNotes', ... /* 55 columns */];
  const publicOnly = publicCols.filter(c => !coreCols.includes(c));
  const coreOnly = coreCols.filter(c => !publicCols.includes(c));
  console.log('Public columns:', publicCols.length);
  console.log('Core columns:', coreCols.length);
  console.log('Public-only:', publicOnly);
  console.log('Core-only:', coreOnly);
  "

  ---
