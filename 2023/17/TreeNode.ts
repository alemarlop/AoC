import { Orientation } from "./Day17.ts";

export class TreeNode {
    public heat: number;
    public position: [number, number];
    public parent: TreeNode | undefined;
    public totalHeat: number;
    public straights: number;
    public children: TreeNode[];
    public alive: boolean;
    public orientation: Orientation;

    constructor(heat: number, position: [number, number], orientation: Orientation, parent?: TreeNode, straights = 0) {
        this.heat = heat;
        this.position = position;
        this.parent = parent;
        this.children = [];
        this.alive = true;
        this.straights = straights;
        this.orientation = orientation;
        this.totalHeat = parent ? parent.totalHeat + heat : heat;
    }

    public addChild(child: TreeNode) {
        this.children.push(child);
    }

    public getChildren(): TreeNode[] {
        return this.children;
    }

    public kill() {
        this.alive = false;
    }

    public pathToRoot(): [number, number][] {
        const path: [number, number][] = [];
        let current: TreeNode | undefined = this;
        while (current !== undefined) {
            path.push(current.position);
            current = current.parent;
        }
        return path;
    }

    public heatToRoot(): number {
        let current: TreeNode | undefined = this;
        let heat = 0;
        while (current !== undefined) {
            heat += current.heat;
            current = current.parent;
        }
        return heat;
    }
}