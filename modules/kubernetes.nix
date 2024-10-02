{
  pkgs,
  ...
}:

{

  imports = [ ./shortcommands.nix ];

  environment.systemPackages = [
    pkgs.kubectl
    pkgs.kubernetes-helm
    pkgs.argocd
  ];

  nagy.shortcommands = {
    k = [ "kubectl" ];
    kg = [
      "kubectl"
      "get"
    ];
    kgp = [
      "kubectl"
      "get"
      "pod"
    ];
    kgd = [
      "kubectl"
      "get"
      "deployment"
    ];
    kgn = [
      "kubectl"
      "get"
      "node"
    ];
    kgpw = [
      "kubectl"
      "get"
      "pod"
      "--watch"
    ];
    kgdw = [
      "kubectl"
      "get"
      "deployment"
      "--watch"
    ];
    kgnw = [
      "kubectl"
      "get"
      "node"
      "--watch"
    ];
    kgsw = [
      "kubectl"
      "get"
      "service"
    ];
    kd = [
      "kubectl"
      "describe"
    ];
    kdp = [
      "kubectl"
      "describe"
      "pod"
    ];
    kdd = [
      "kubectl"
      "describe"
      "deployment"
    ];
    kdn = [
      "kubectl"
      "describe"
      "node"
    ];
    kc = [
      "kubectl"
      "create"
    ];
    kcp = [
      "kubectl"
      "create"
      "pod"
    ];
    kcd = [
      "kubectl"
      "create"
      "deployment"
    ];
    kcj = [
      "kubectl"
      "create"
      "job"
    ];
    kcn = [
      "kubectl"
      "create"
      "namespace"
    ];
    ke = [
      "kubectl"
      "exec"
    ];
    keti = [
      "kubectl"
      "exec"
      "-it"
    ];
    kl = [
      "kubectl"
      "label"
    ];
    kw = [
      "kubectl"
      "wait"
    ];
    kr = [
      "kubectl"
      "run"
    ];

    A = [ "argocd" ];
    Aa = [
      "argocd"
      "app"
    ];
    Aal = [
      "argocd"
      "app"
      "list"
    ];
    Ac = [
      "argocd"
      "cluster"
    ];
    Acl = [
      "argocd"
      "cluster"
      "list"
    ];

    Ar = [
      "argocd"
      "repo"
    ];
    Arl = [
      "argocd"
      "repo"
      "list"
    ];

  };

}
