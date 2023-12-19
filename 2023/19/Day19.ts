enum Category {
    x = 'x',
    m = 'm',
    a = 'a',
    s = 's',
}

interface Workflow {
    id: string;
    conditions: ((x: number, k: string) => string | undefined)[];
    otherwise: string;
}

interface Part {
    currentWorkflowId: string | 'A' | 'R';
    [Category.x]: number;
    [Category.m]: number;
    [Category.a]: number;
    [Category.s]: number;
}

const fileName: string = Deno.args[0];
const fileContent: string = await Deno.readTextFile(fileName);

const [rawWorkflows, rawParts] = fileContent.split('\n\n');

const workflows: Workflow[] = rawWorkflows.split('\n').map((rawWorkflow) => {
    const [id, rest] = rawWorkflow.split('{');
    const workflow = {
        id,
        conditions: [],
        otherwise: '',
    } as Workflow;
    rest.substring(0, rest.length - 1).split(',').forEach((condition) => {
        const [c, target] = condition.split(':');
        const sign = c.charAt(1)
        const numAsString = (c.match(/\d+/) as RegExpMatchArray);
        if (!numAsString) {
            workflow.otherwise = condition;
            return;
        }
        const num = Number(numAsString[0]);
        const func = (x: number, k: string) => {
            if (sign === '<') {
                return k === c.charAt(0) ? x < num ? target : undefined : undefined;
            } else if (sign === '>') {
                return k === c.charAt(0) ? x > num ? target : undefined : undefined;
            }
        }
        workflow.conditions.push(func);
    });
    return workflow;
});

const parts: Part[] = rawParts.split('\n').map((rawPart) => {
    const [x, m, a, s] = (rawPart.match(/\d+/g) as RegExpMatchArray) as string[];
    return {
        currentWorkflowId: 'in',
        [Category.x]: Number(x),
        [Category.m]: Number(m),
        [Category.a]: Number(a),
        [Category.s]: Number(s),
    } as Part;
});

function executeWorkflow(part: Part, workflows: Workflow[]): Part {
    if (part.currentWorkflowId === 'A' || part.currentWorkflowId === 'R') {
        return part;
    }
    const workflow = workflows.find((workflow) => workflow.id === part.currentWorkflowId);
    if (!workflow) {
        throw new Error('No workflow found');
    }
    for (const condition of workflow.conditions) {
        const nextWorkflowId = condition(part.x, 'x') || condition(part.m, 'm') || condition(part.a, 'a') || condition(part.s, 's');
        if (nextWorkflowId) {
            return {
                ...part,
                currentWorkflowId: nextWorkflowId,
            };
        }
    }
    return {
        ...part,
        currentWorkflowId: workflow.otherwise,
    };
}

function executeDeepWorkflow(part: Part, workflows: Workflow[]): Part {
    let nextPart = part;
    while (nextPart.currentWorkflowId !== 'A' && nextPart.currentWorkflowId !== 'R') {
        nextPart = executeWorkflow(nextPart, workflows);
    }
    return nextPart;
}

function executeAllParts(parts: Part[], workflows: Workflow[]): Part[] {
    return parts.map((part) => executeDeepWorkflow(part, workflows));
}

console.log('Part 1:', executeAllParts(parts, workflows).reduce((acc, part) => {
    if (part.currentWorkflowId === 'A') {
        return acc + part.x + part.m + part.a + part.s;
    }
    return acc;
}, 0));

// function minimizeRules(workflows: Workflow[]): any {
//     const otherwiseA = workflows.filter((workflow) => workflow.id === 'A');
//     // para entrar por el otherwise, el resto de reglas se han tenido que no cumplir
    
// }
